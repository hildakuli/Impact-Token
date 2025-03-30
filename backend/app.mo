import HashMap "mo:base/HashMap";
import Text "mo:base/Text";
import Principal "mo:base/Principal";
import Nat "mo:base/Nat";
import Int "mo:base/Int";
import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Array "mo:base/Array";

actor ImpactTokenSystem {
    // Tipe data untuk profil kontributor (tanpa Buffer)
    type Contributor = {
        id: Principal;
        name: Text;
        totalImpactScore: Nat;
        impactTokens: Nat;
        contributionHistory: [Contribution];
    };

    // Kategori kontribusi
    type ContributionCategory = {
        #Education;
        #Environment;
        #Community;
        #Healthcare;
        #Technology;
        #Humanitarian;
    };

    // Detail kontribusi
    type Contribution = {
        description: Text;
        category: ContributionCategory;
        impactScore: Nat;
        timestamp: Int;
        verificationStatus: VerificationStatus;
        verifiers: [Principal];
    };

    // Status verifikasi
    type VerificationStatus = {
        #Pending;
        #Verified;
        #Rejected;
    };

    // Penyimpanan data
    private var contributors = HashMap.HashMap<Principal, Contributor>(10, Principal.equal, Principal.hash);
    private var globalContributionPool : Nat = 0;

    // Mendaftarkan kontributor baru
    public shared(msg) func registerContributor(name: Text) : async Result.Result<Principal, Text> {
        if (contributors.get(msg.caller) != null) {
            return #err("Contributor already registered");
        };

        let newContributor : Contributor = {
            id = msg.caller;
            name = name;
            totalImpactScore = 0;
            impactTokens = 0;
            contributionHistory = [];
        };

        contributors.put(msg.caller, newContributor);
        #ok(msg.caller)
    };

    // Mencatat kontribusi baru
    public shared(msg) func logContribution(
        description: Text, 
        category: ContributionCategory,
        impactScore: Nat
    ) : async Result.Result<Contribution, Text> {
        switch (contributors.get(msg.caller)) {
            case null { return #err("Contributor not registered"); };
            case (?contributor) {
                let newContribution : Contribution = {
                    description = description;
                    category = category;
                    impactScore = impactScore;
                    timestamp = Time.now();
                    verificationStatus = #Pending;
                    verifiers = [];
                };

                let updatedContributor : Contributor = {
                    id = contributor.id;
                    name = contributor.name;
                    totalImpactScore = contributor.totalImpactScore + impactScore;
                    impactTokens = contributor.impactTokens;
                    contributionHistory = 
                        Array.append(contributor.contributionHistory, [newContribution]);
                };

                contributors.put(msg.caller, updatedContributor);
                globalContributionPool += impactScore;
                #ok(newContribution)
            };
        }
    };

    // Verifikasi kontribusi
    public shared(msg) func verifyContribution(
        contributorId: Principal,
        contributionIndex: Nat
    ) : async Result.Result<Contribution, Text> {
        switch (contributors.get(contributorId)) {
            case null { return #err("Contributor not found"); };
            case (?contributor) {
                if (contributionIndex >= contributor.contributionHistory.size()) {
                    return #err("Invalid contribution index");
                };

                var updatedContribution = contributor.contributionHistory[contributionIndex];
                
                // Cek apakah sudah diverifikasi
                if (updatedContribution.verificationStatus != #Pending) {
                    return #err("Contribution already verified/rejected");
                };

                // Tambahkan verifier
                let updatedVerifiers = 
                    Array.append(updatedContribution.verifiers, [msg.caller]);

                // Update status jika sudah cukup verifier
                let newVerificationStatus = 
                    if (updatedVerifiers.size() >= 3) #Verified else #Pending;

                // Buat kontribusi yang diperbarui
                updatedContribution := {
                    description = updatedContribution.description;
                    category = updatedContribution.category;
                    impactScore = updatedContribution.impactScore;
                    timestamp = updatedContribution.timestamp;
                    verificationStatus = newVerificationStatus;
                    verifiers = updatedVerifiers;
                };

                // Update riwayat kontribusi
                let updatedContributionHistory = 
                    Array.thaw(contributor.contributionHistory);
                updatedContributionHistory[contributionIndex] := updatedContribution;

                // Update kontributor
                let updatedContributor : Contributor = {
                    id = contributor.id;
                    name = contributor.name;
                    totalImpactScore = contributor.totalImpactScore;
                    impactTokens = 
                        if (newVerificationStatus == #Verified) 
                        contributor.impactTokens + updatedContribution.impactScore 
                        else contributor.impactTokens;
                    contributionHistory = Array.freeze(updatedContributionHistory);
                };

                contributors.put(contributorId, updatedContributor);
                #ok(updatedContribution)
            };
        }
    };

    // Menukar token impact
    public shared(msg) func exchangeImpactTokens(
        amount: Nat, 
        serviceDescription: Text
    ) : async Result.Result<Nat, Text> {
        switch (contributors.get(msg.caller)) {
            case null { return #err("Contributor not registered"); };
            case (?contributor) {
                if (contributor.impactTokens < amount) {
                    return #err("Insufficient impact tokens");
                };

                let updatedContributor : Contributor = {
                    id = contributor.id;
                    name = contributor.name;
                    totalImpactScore = contributor.totalImpactScore;
                    impactTokens = contributor.impactTokens - amount;
                    contributionHistory = contributor.contributionHistory;
                };

                contributors.put(msg.caller, updatedContributor);
                #ok(amount)
            };
        }
    };

    // Mendapatkan profil kontributor (versi query yang aman)
    public query func getContributorProfile(contributorId: Principal) : async ?{
        id: Principal;
        name: Text;
        totalImpactScore: Nat;
        impactTokens: Nat;
        contributionHistoryCount: Nat;
    } {
        switch (contributors.get(contributorId)) {
            case null { null };
            case (?contributor) {
                ?{
                    id = contributor.id;
                    name = contributor.name;
                    totalImpactScore = contributor.totalImpactScore;
                    impactTokens = contributor.impactTokens;
                    contributionHistoryCount = contributor.contributionHistory.size();
                }
            };
        }
    };

    // Mendapatkan riwayat kontribusi
    public query func getContributionHistory(contributorId: Principal) : async [{
        description: Text;
        category: ContributionCategory;
        impactScore: Nat;
        timestamp: Int;
        verificationStatus: VerificationStatus;
        verifiersCount: Nat;
    }] {
        switch (contributors.get(contributorId)) {
            case null { [] };
            case (?contributor) { 
                contributor.contributionHistory 
                    |> Iter.fromArray(_) 
                    |> Iter.map(_, func(contribution: Contribution) : {
                        description: Text;
                        category: ContributionCategory;
                        impactScore: Nat;
                        timestamp: Int;
                        verificationStatus: VerificationStatus;
                        verifiersCount: Nat;
                    } {
                        {
                            description = contribution.description;
                            category = contribution.category;
                            impactScore = contribution.impactScore;
                            timestamp = contribution.timestamp;
                            verificationStatus = contribution.verificationStatus;
                            verifiersCount = contribution.verifiers.size();
                        }
                    }) 
                    |> Iter.toArray(_)
            };
        }
    };
}
