;; NFTs, Badges & SBTs (Soulbound Tokens) Contract
;; Handles achievement NFTs and soulbound tokens for fitness/identity verification

;; Implementing proper SIP-009 NFT standard instead of custom trait
(define-non-fungible-token achievement-nft uint)
(define-non-fungible-token soulbound-token uint)

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u401))
(define-constant ERR-NOT-FOUND (err u404))
(define-constant ERR-ALREADY-EXISTS (err u409))
(define-constant ERR-INVALID-TYPE (err u400))
(define-constant ERR-SBT-NON-TRANSFERABLE (err u403))
(define-constant ERR-INVALID-PROOF (err u402))
(define-constant ERR-INSUFFICIENT-ACHIEVEMENTS (err u405))

;; Achievement Types
(define-constant ACHIEVEMENT-FIRST-MARATHON u1)
(define-constant ACHIEVEMENT-10K-STEPS-STREAK u2)
(define-constant ACHIEVEMENT-WEIGHT-LOSS-GOAL u3)
(define-constant ACHIEVEMENT-CONSISTENCY-BADGE u4)
(define-constant ACHIEVEMENT-DISTANCE-MILESTONE u5)

;; SBT Types
(define-constant SBT-IDENTITY-VERIFIED u1)
(define-constant SBT-KYC-VERIFIED u2)
(define-constant SBT-FITNESS-TRAINER u3)
(define-constant SBT-MEDICAL-CLEARANCE u4)

;; Data Variables
(define-data-var next-nft-id uint u1)
(define-data-var next-sbt-id uint u1)
(define-data-var contract-uri (string-ascii 256) "https://api.fitness-app.com/metadata/")

;; Data Maps
(define-map achievement-nfts uint {
  owner: principal,
  achievement-type: uint,
  metadata: (string-ascii 500),
  minted-at: uint,
  transferable: bool
})

(define-map soulbound-tokens uint {
  owner: principal,
  sbt-type: uint,
  title: (string-ascii 100),
  proof: (string-ascii 500),
  issued-at: uint,
  issuer: principal,
  verified: bool
})

(define-map user-achievements principal (list 50 uint))
(define-map user-sbt-tokens principal (list 20 uint))
(define-map achievement-metadata uint {
  name: (string-ascii 100),
  description: (string-ascii 300),
  image-uri: (string-ascii 200),
  requirements: (string-ascii 200)
})

(define-map sbt-metadata uint {
  name: (string-ascii 100),
  description: (string-ascii 300),
  credential-type: (string-ascii 100),
  validity-period: (optional uint)
})

(define-map authorized-minters principal bool)
(define-map achievement-counters {user: principal, type: uint} uint)

;; Initialize achievement metadata
(map-set achievement-metadata ACHIEVEMENT-FIRST-MARATHON {
  name: "First Marathon Finisher",
  description: "Completed your first marathon distance (26.2 miles)",
  image-uri: "https://api.fitness-app.com/images/marathon-badge.png",
  requirements: "Complete 26.2 miles in a single activity"
})

(map-set achievement-metadata ACHIEVEMENT-10K-STEPS-STREAK {
  name: "10K Steps Streak Master",
  description: "Maintained 10,000+ steps daily for 30 consecutive days",
  image-uri: "https://api.fitness-app.com/images/steps-streak.png",
  requirements: "10,000+ steps daily for 30 days"
})

(map-set achievement-metadata ACHIEVEMENT-WEIGHT-LOSS-GOAL {
  name: "Weight Loss Champion",
  description: "Successfully reached your weight loss goal",
  image-uri: "https://api.fitness-app.com/images/weight-loss.png",
  requirements: "Achieve personal weight loss target"
})

;; Initialize SBT metadata
(map-set sbt-metadata SBT-IDENTITY-VERIFIED {
  name: "Identity Verified",
  description: "Government-issued ID verification completed",
  credential-type: "Identity Verification",
  validity-period: (some u525600) ;; 1 year in blocks
})

(map-set sbt-metadata SBT-KYC-VERIFIED {
  name: "KYC Verified",
  description: "Know Your Customer verification completed",
  credential-type: "KYC Compliance",
  validity-period: (some u525600)
})

;; SIP-009 compliant NFT functions
(define-read-only (get-last-token-id)
  (ok (- (var-get next-nft-id) u1))
)

(define-read-only (get-token-uri (token-id uint))
  (match (map-get? achievement-nfts token-id)
    nft (ok (some (concat (var-get contract-uri) (uint-to-ascii token-id))))
    (err ERR-NOT-FOUND)
  )
)

(define-read-only (get-owner (token-id uint))
  (ok (nft-get-owner? achievement-nft token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
  (let ((nft (unwrap! (map-get? achievement-nfts token-id) ERR-NOT-FOUND)))
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (asserts! (is-eq (some sender) (nft-get-owner? achievement-nft token-id)) ERR-NOT-AUTHORIZED)
    (asserts! (get transferable nft) ERR-SBT-NON-TRANSFERABLE)
    
    (try! (nft-transfer? achievement-nft token-id sender recipient))
    (map-set achievement-nfts token-id (merge nft {owner: recipient}))
    (print {action: "transfer", token-id: token-id, from: sender, to: recipient})
    (ok true)
  )
)

;; Achievement NFT Functions
(define-public (mint-achievement-nft (user-id principal) (achievement-type uint) (metadata (string-ascii 500)))
  (let (
    (token-id (var-get next-nft-id))
    (current-achievements (default-to (list) (map-get? user-achievements user-id)))
  )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (default-to false (map-get? authorized-minters tx-sender))) ERR-NOT-AUTHORIZED)
    (asserts! (is-valid-achievement-type achievement-type) ERR-INVALID-TYPE)
    
    ;; Mint using SIP-009 standard
    (try! (nft-mint? achievement-nft token-id user-id))
    
    ;; Store achievement data
    (map-set achievement-nfts token-id {
      owner: user-id,
      achievement-type: achievement-type,
      metadata: metadata,
      minted-at: block-height,
      transferable: true
    })
    
    ;; Update user's achievement list
    (map-set user-achievements user-id (unwrap! (as-max-len? (append current-achievements token-id) u50) ERR-INSUFFICIENT-ACHIEVEMENTS))
    
    ;; Increment achievement counter
    (map-set achievement-counters {user: user-id, type: achievement-type} 
             (+ (default-to u0 (map-get? achievement-counters {user: user-id, type: achievement-type})) u1))
    
    ;; Increment next token ID
    (var-set next-nft-id (+ token-id u1))
    
    (print {action: "mint-achievement", token-id: token-id, user: user-id, type: achievement-type})
    (ok token-id)
  )
)

;; Soulbound Token Functions
(define-public (mint-sbt (user-id principal) (sbt-type uint) (title (string-ascii 100)) (proof (string-ascii 500)))
  (let (
    (sbt-id (var-get next-sbt-id))
    (current-sbts (default-to (list) (map-get? user-sbt-tokens user-id)))
  )
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (default-to false (map-get? authorized-minters tx-sender))) ERR-NOT-AUTHORIZED)
    (asserts! (is-valid-sbt-type sbt-type) ERR-INVALID-TYPE)
    (asserts! (> (len proof) u0) ERR-INVALID-PROOF)
    
    ;; Mint SBT using separate NFT definition
    (try! (nft-mint? soulbound-token sbt-id user-id))
    
    ;; Store SBT data
    (map-set soulbound-tokens sbt-id {
      owner: user-id,
      sbt-type: sbt-type,
      title: title,
      proof: proof,
      issued-at: block-height,
      issuer: tx-sender,
      verified: true
    })
    
    ;; Update user's SBT list
    (map-set user-sbt-tokens user-id (unwrap! (as-max-len? (append current-sbts sbt-id) u20) ERR-INSUFFICIENT-ACHIEVEMENTS))
    
    ;; Increment next SBT ID
    (var-set next-sbt-id (+ sbt-id u1))
    
    (print {action: "mint-sbt", sbt-id: sbt-id, user: user-id, type: sbt-type, title: title})
    (ok sbt-id)
  )
)

;; Read-only Functions
(define-read-only (get-achievement-nft (token-id uint))
  (map-get? achievement-nfts token-id)
)

(define-read-only (get-soulbound-token (sbt-id uint))
  (map-get? soulbound-tokens sbt-id)
)

(define-read-only (get-user-achievements (user principal))
  (default-to (list) (map-get? user-achievements user))
)

(define-read-only (get-user-sbt-tokens (user principal))
  (default-to (list) (map-get? user-sbt-tokens user))
)

(define-read-only (get-achievement-count (user principal) (achievement-type uint))
  (default-to u0 (map-get? achievement-counters {user: user, type: achievement-type}))
)

(define-read-only (get-achievement-metadata (achievement-type uint))
  (map-get? achievement-metadata achievement-type)
)

(define-read-only (get-sbt-metadata (sbt-type uint))
  (map-get? sbt-metadata sbt-type)
)

(define-read-only (is-sbt-valid (sbt-id uint))
  (match (map-get? soulbound-tokens sbt-id)
    sbt (let ((metadata (unwrap! (map-get? sbt-metadata (get sbt-type sbt)) false)))
          (match (get validity-period metadata)
            validity-blocks (< (- block-height (get issued-at sbt)) validity-blocks)
            true ;; No expiration
          ))
    false
  )
)

;; Validation Functions
(define-read-only (is-valid-achievement-type (achievement-type uint))
  (or (is-eq achievement-type ACHIEVEMENT-FIRST-MARATHON)
      (is-eq achievement-type ACHIEVEMENT-10K-STEPS-STREAK)
      (is-eq achievement-type ACHIEVEMENT-WEIGHT-LOSS-GOAL)
      (is-eq achievement-type ACHIEVEMENT-CONSISTENCY-BADGE)
      (is-eq achievement-type ACHIEVEMENT-DISTANCE-MILESTONE))
)

(define-read-only (is-valid-sbt-type (sbt-type uint))
  (or (is-eq sbt-type SBT-IDENTITY-VERIFIED)
      (is-eq sbt-type SBT-KYC-VERIFIED)
      (is-eq sbt-type SBT-FITNESS-TRAINER)
      (is-eq sbt-type SBT-MEDICAL-CLEARANCE))
)

;; Admin Functions
(define-public (add-authorized-minter (minter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-set authorized-minters minter true)
    (ok true)
  )
)

(define-public (remove-authorized-minter (minter principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (map-delete authorized-minters minter)
    (ok true)
  )
)

(define-public (update-contract-uri (new-uri (string-ascii 256)))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (var-set contract-uri new-uri)
    (ok true)
  )
)

(define-public (revoke-sbt (sbt-id uint))
  (let ((sbt (unwrap! (map-get? soulbound-tokens sbt-id) ERR-NOT-FOUND)))
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (is-eq tx-sender (get issuer sbt))) ERR-NOT-AUTHORIZED)
    
    (map-set soulbound-tokens sbt-id (merge sbt {verified: false}))
    (print {action: "revoke-sbt", sbt-id: sbt-id})
    (ok true)
  )
)

;; Batch Operations
(define-public (batch-mint-achievements (recipients (list 10 {user: principal, type: uint, metadata: (string-ascii 500)})))
  (begin
    (asserts! (or (is-eq tx-sender CONTRACT-OWNER) 
                  (default-to false (map-get? authorized-minters tx-sender))) ERR-NOT-AUTHORIZED)
    (ok (map mint-achievement-batch recipients))
  )
)

(define-private (mint-achievement-batch (recipient {user: principal, type: uint, metadata: (string-ascii 500)}))
  (mint-achievement-nft (get user recipient) (get type recipient) (get metadata recipient))
)

;; Helper function for uint to ascii conversion (simplified)
(define-read-only (uint-to-ascii (value uint))
  (if (is-eq value u0) "0"
  (if (is-eq value u1) "1"
  (if (is-eq value u2) "2"
  (if (is-eq value u3) "3"
  (if (is-eq value u4) "4"
  (if (is-eq value u5) "5"
  "unknown"))))))
)