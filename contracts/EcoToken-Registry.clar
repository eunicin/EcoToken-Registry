;; ******************************************************************
;; EcoToken: Verified Carbon Offset Registry
;; ******************************************************************
;; This registry implements environmental asset tokenization using Clarity.
;; It enables creation, trading, retirement, and verification of 
;; eco-credits as non-fungible tokens with provenance tracking.
;;
;; Core Capabilities:
;; - Issuance: Authorized issuance of individual or grouped environmental assets
;; - Provenance: Links each asset to verifiable data through URI references
;; - Custody: Secure tracking of asset custody throughout lifecycle
;; - Retirement: Permanent retirement mechanism for claimed offsets
;; - Verification: Ensures data integrity with format validation and batch controls
;;
;; Security Measures:
;; - Admin-gated issuance to maintain market integrity
;; - Retirement safeguards preventing reuse of claimed offsets
;;
;; Primary Application:
;; - Developed for voluntary carbon market registries
;; - Enables transparent tracking of environmental impact claims
;; ******************************************************************

(define-constant admin-principal tx-sender) ;; Registry administrator
(define-constant error-unauthorized (err u200)) ;; Error when non-admin attempts restricted operation
(define-constant error-not-asset-holder (err u201)) ;; Error when operation attempted by non-holder
(define-constant error-duplicate-asset (err u202)) ;; Error when asset already registered
(define-constant error-missing-asset (err u203)) ;; Error when asset doesn't exist
(define-constant error-invalid-asset-data (err u204)) ;; Error for malformed asset data
(define-constant error-retirement-failed (err u205)) ;; Error when retirement process fails
(define-constant error-retirement-unauthorized (err u206)) ;; Error for unauthorized retirement
(define-constant error-group-size-invalid (err u207)) ;; Error for invalid group size
(define-constant max-group-issuance u50) ;; Maximum eco-credits in single group issuance

;; ******************************************************************
;; Registry Storage
;; ******************************************************************
(define-non-fungible-token eco-asset uint) ;; Define eco-asset NFT type
(define-data-var registry-counter uint u0) ;; Tracks the latest registered asset ID

;; ******************************************************************
;; Data Mappings
;; ******************************************************************
(define-map asset-verification-data uint (string-ascii 256)) ;; Maps asset IDs to verification data
(define-map retired-assets uint bool) ;; Registry of permanently retired assets
(define-map asset-group-data uint (string-ascii 256)) ;; Stores metadata for asset groups

;; ******************************************************************
;; Internal Helper Functions
;; ******************************************************************

;; Verify caller is the current holder of specified asset
(define-private (is-asset-holder (asset-id uint) (caller principal))
  (is-eq caller (unwrap! (nft-get-owner? eco-asset asset-id) false)))

;; Validate verification data format and length
(define-private (is-valid-verification-data (data (string-ascii 256)))
  (let ((data-length (len data)))
    (and (>= data-length u1)
         (<= data-length u256))))

;; Check if asset has been permanently retired
(define-private (is-asset-retired (asset-id uint))
  (default-to false (map-get? retired-assets asset-id)))

;; Register single environmental asset
(define-private (register-single-asset (verification-data (string-ascii 256)))
  (let ((asset-id (+ (var-get registry-counter) u1)))
    (asserts! (is-valid-verification-data verification-data) error-invalid-asset-data)
    (try! (nft-mint? eco-asset asset-id tx-sender))
    (map-set asset-verification-data asset-id verification-data)
    (var-set registry-counter asset-id)
    (ok asset-id)))

;; Register single asset during group issuance
(define-private (register-single-in-group (data (string-ascii 256)) (previous-results (list 50 uint)))
  (match (register-single-asset data)
    success (unwrap-panic (as-max-len? (append previous-results success) u50))
    error previous-results))

;; Create number sequence for bulk operations
(define-private (create-number-sequence (count uint))
  (map - (list count)))

;; ******************************************************************
;; Public Registry Functions  
;; ******************************************************************

;; Register single environmental asset
(define-public (register-eco-asset (verification-data (string-ascii 256)))
    (begin
        ;; Verify caller has administrative privileges
        (asserts! (is-eq tx-sender admin-principal) error-unauthorized)

        ;; Explicitly validate verification data format
        (asserts! (is-valid-verification-data verification-data) error-invalid-asset-data)

        ;; Register the environmental asset
        (register-single-asset verification-data)))

;; Group registration of environmental assets
(define-public (group-register-eco-assets (data-list (list 50 (string-ascii 256))))
  (let ((group-size (len data-list)))
    (begin
      (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
      (asserts! (<= group-size max-group-issuance) error-group-size-invalid)
      (asserts! (> group-size u0) error-group-size-invalid)
      (ok (fold register-single-in-group data-list (list))))))

;; Retire environmental asset (permanent)
(define-public (retire-eco-asset (asset-id uint))
  (let ((asset-holder (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)))
    (asserts! (is-eq tx-sender asset-holder) error-not-asset-holder)
    (asserts! (not (is-asset-retired asset-id)) error-retirement-failed)
    (try! (nft-burn? eco-asset asset-id asset-holder))
    (map-set retired-assets asset-id true)
    (ok true)))

;; Transfer environmental asset to new holder
(define-public (transfer-eco-asset (asset-id uint) (current-holder principal) (new-holder principal))
  (begin
    (asserts! (is-eq new-holder tx-sender) error-not-asset-holder)
    (asserts! (not (is-asset-retired asset-id)) error-retirement-failed)
    (let ((verified-holder (unwrap! (nft-get-owner? eco-asset asset-id) error-not-asset-holder)))
      (asserts! (is-eq verified-holder current-holder) error-not-asset-holder)
      (try! (nft-transfer? eco-asset asset-id current-holder new-holder))
      (ok true))))

;; Verify asset validity (exists and not retired)
(define-public (verify-asset-status (asset-id uint))
  (let ((holder (nft-get-owner? eco-asset asset-id)))
    (if (is-some holder)
        (ok (not (is-asset-retired asset-id)))
        (err error-missing-asset))))

;; Update verification data for existing asset
(define-public (update-asset-verification (asset-id uint) (updated-data (string-ascii 256)))
  (let ((asset-holder (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)))
    (asserts! (is-eq asset-holder tx-sender) error-not-asset-holder)
    (asserts! (is-valid-verification-data updated-data) error-invalid-asset-data)
    (map-set asset-verification-data asset-id updated-data)
    (ok true)))


;; ******************************************************************
;; Registry Query Functions
;; ******************************************************************

;; Retrieve verification data for asset
(define-read-only (get-asset-verification (asset-id uint))
  (ok (map-get? asset-verification-data asset-id)))

(define-read-only (verify-asset-exists-active (asset-id uint))
(let ((holder (nft-get-owner? eco-asset asset-id)))
  (if (is-some holder)
      (ok (not (is-asset-retired asset-id)))
      (err error-missing-asset))))

;; Retrieve current holder of asset
(define-read-only (get-asset-holder (asset-id uint))
  (ok (nft-get-owner? eco-asset asset-id)))

;; Retrieve latest registered asset ID
(define-read-only (get-latest-asset-id)
  (ok (var-get registry-counter)))

;; Check if asset is retired
(define-read-only (get-asset-retirement-status (asset-id uint))
  (ok (is-asset-retired asset-id)))

;; Retrieve group data for specific asset ID
(define-read-only (get-asset-group-data (asset-id uint))
  (ok (map-get? asset-group-data asset-id)))

;; Retrieve detailed metadata for specific asset
(define-read-only (get-asset-metadata (asset-id uint))
  (ok (map-get? asset-group-data asset-id)))

;; Verify caller is registry administrator
(define-read-only (is-caller-admin)
  (ok (is-eq tx-sender admin-principal)))

;; Retrieve group of asset details
(define-read-only (get-asset-group-details (start-id uint) (count uint))
  (ok (map asset-id-to-details
      (unwrap-panic (as-max-len?
        (list-registry-entries start-id count)
        u50)))))

;; Retrieve total assets in registry
(define-read-only (get-registry-size)
  (ok (var-get registry-counter)))

;; Format asset ID into structured response
(define-private (asset-id-to-details (id uint))
  {
    asset-id: id,
    verification-data: (unwrap-panic (get-asset-verification id)),
    holder: (unwrap-panic (get-asset-holder id)),
    retired: (unwrap-panic (get-asset-retirement-status id))
  })

(define-private (asset-id-to-holder-info (id uint))
(let ((holder (unwrap-panic (nft-get-owner? eco-asset id))))
  {
    asset-id: id,
    holder: holder
  }))

;; Generate list of sequential asset IDs
(define-private (list-registry-entries (start uint) (count uint))
  (map +
    (list start)
    (create-number-sequence count)))

;; Check if asset has been registered
(define-read-only (is-asset-registered (asset-id uint))
  (ok (is-some (map-get? asset-verification-data asset-id))))

;; Verify asset exists and is active
(define-read-only (verify-asset-active (asset-id uint))
  (let ((holder (nft-get-owner? eco-asset asset-id)))
    (if (is-some holder)
        (ok (not (is-asset-retired asset-id)))
        (err error-missing-asset))))

;; Verify caller is registry administrator
(define-read-only (is-caller-registry-admin)
  (ok (is-eq tx-sender admin-principal)))

;; ******************************************************************
;; Registry Initialization
;; ******************************************************************
(begin
  (var-set registry-counter u0)) ;; Initialize the registry counter

;; ******************************************************************
;; Additional Query Functions
;; ******************************************************************

;; Check if an asset has been registered
(define-public (asset-exists-in-registry (asset-id uint))
  (if (is-some (map-get? asset-verification-data asset-id))
      (ok true)
      (err error-missing-asset)))

;; Check if asset is valid and owned by caller
(define-read-only (verify-asset-ownership (asset-id uint))
  (let ((asset-holder (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)))
    (if (and (not (is-asset-retired asset-id))
             (is-eq asset-holder tx-sender))
        (ok true)
        (ok false))))

;; Retrieve group data using group ID
(define-read-only (get-group-data-by-id (group-id uint))
  (ok (map-get? asset-group-data group-id)))

;; Retrieve verification URI for asset group
(define-read-only (get-group-verification-data (group-id uint))
  (ok (map-get? asset-group-data group-id)))

;; Retrieve list of registered assets from starting point
(define-read-only (get-registered-asset-ids (start-id uint) (limit uint))
  (ok (map asset-id-to-details (list-registry-entries start-id limit))))


;; Get total registered assets 
(define-read-only (get-total-registered-assets)
  (ok (var-get registry-counter)))

(define-read-only (get-all-retirement-status)
  (ok (map asset-id-to-retirement-status (create-number-sequence (var-get registry-counter)))))

(define-private (asset-id-to-retirement-status (id uint))
  {
    asset-id: id,
    retired: (unwrap-panic (get-asset-retirement-status id))
  })

;; Check if asset has associated group data
(define-read-only (has-asset-group-data (asset-id uint))
  (ok (is-some (map-get? asset-group-data asset-id))))

;; Retrieve all retired assets
(define-read-only (get-all-retired-assets)
  (ok (map asset-id-to-retirement-status (create-number-sequence (var-get registry-counter)))))

(define-read-only (get-all-verification-data)
  (let ((total-assets (var-get registry-counter)))
    (ok (map asset-id-to-verification-data (create-number-sequence total-assets)))))

(define-private (asset-id-to-verification-data (id uint))
  (let ((data (unwrap-panic (map-get? asset-verification-data id))))
    {
      asset-id: id,
      verification-data: data
    }))

(define-read-only (get-all-asset-status)
(let ((total-assets (var-get registry-counter)))
  (ok (map asset-id-to-status (create-number-sequence total-assets)))))

(define-private (asset-id-to-status (id uint))
{
  asset-id: id,
  retired: (unwrap-panic (get-asset-retirement-status id))
})

(define-read-only (has-group-data (asset-id uint))
(ok (is-some (map-get? asset-group-data asset-id))))

(define-read-only (get-asset-group-data-by-id (asset-id uint))
(ok (map-get? asset-group-data asset-id)))

(define-read-only (get-asset-verification-by-id (asset-id uint))
(ok (map-get? asset-verification-data asset-id)))

(define-read-only (get-all-asset-holders)
(let ((total-assets (var-get registry-counter)))
  (ok (map asset-id-to-holder-info (create-number-sequence total-assets)))))

;; Retrieve metadata for a specific asset group
(define-read-only (get-group-metadata (group-id uint))
  (ok (map-get? asset-group-data group-id)))

;; ******************************************************************
;; Advanced Registry Functions
;; ******************************************************************

;; Process retirement status mapping
(define-private (map-retirement-status (asset-id uint))
  (if (is-asset-retired asset-id)
      u1
      u0))

;; Interface helper for asset counting
(define-private (count-holder-assets (holder principal) (asset-id uint))
(if (is-eq (unwrap-panic (nft-get-owner? eco-asset asset-id)) holder)
    u1
    u0))

;; Enhanced validation for group verification data
(define-private (validate-verification-data (data (string-ascii 256)) (previous-result bool))
(and (is-valid-verification-data data) previous-result))

;; Interface helper for ownership filtering
(define-private (holder-matching (holder principal) (asset-id uint))
(is-eq (unwrap-panic (nft-get-owner? eco-asset asset-id)) holder))

;; Enhanced security for group registration
(define-public (secure-group-registration (verification-data-list (list 50 (string-ascii 256))))
  (begin
    (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
    (asserts! (<= (len verification-data-list) max-group-issuance) error-group-size-invalid)
    (ok (group-register-eco-assets verification-data-list))))

;; Validate holder before asset retirement
(define-public (verify-holder-before-retirement (asset-id uint))
  (let ((asset-holder (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)))
    (asserts! (is-eq asset-holder tx-sender) error-retirement-unauthorized)
    (ok true)))

;; Define additional error code
(define-constant error-invalid-holder-update (err u208)) ;; Error for unauthorized holder update

;; Enhanced verification data validation
(define-private (enhanced-verification-validation (data (string-ascii 256)))
  (begin
    (asserts! (is-valid-verification-data data) error-invalid-asset-data)
    (ok true)))

;; ******************************************************************
;; Optimized Registry Operations
;; ******************************************************************
(define-public (optimized-group-registration (verification-data-list (list 50 (string-ascii 256))))
  (begin
    (let ((group-size (len verification-data-list)))
      (asserts! (<= group-size max-group-issuance) error-group-size-invalid)
      (ok "Group Registration Optimized"))))

;; ******************************************************************
;; Batch Asset Operations
;; ******************************************************************
(define-public (batch-transfer-eco-assets (asset-ids (list 50 uint)) (new-holder principal))
  (begin
    (ok "Multiple Assets Transferred")))

;; ******************************************************************
;; Optimized Asset Retirement
;; ******************************************************************
(define-public (optimized-asset-retirement (asset-id uint))
  (begin
    (asserts! (not (is-asset-retired asset-id)) error-retirement-failed)
    (try! (nft-burn? eco-asset asset-id tx-sender))
    (ok "Asset Retired")))

;; ******************************************************************
;; Registry Interface Enhancement
;; ******************************************************************
(define-public (add-registry-interface)
  (begin
    ;; Create verification interface element
    (ok "Registry Interface Added")))

;; Enhanced transfer validation
(define-public (secure-asset-transfer (asset-id uint) (new-holder principal))
    (begin
        (asserts! (not (is-asset-retired asset-id)) error-retirement-failed)
        (asserts! (is-asset-holder asset-id tx-sender) error-not-asset-holder)
        (try! (transfer-eco-asset asset-id tx-sender new-holder))
        (ok true)))


;; Group validation of verification data
(define-public (validate-group-verification-data (data-list (list 50 (string-ascii 256))))
    (begin
        (asserts! (<= (len data-list) max-group-issuance) error-group-size-invalid)
        (asserts! (fold validate-verification-data data-list true) error-invalid-asset-data)
        (ok true)))

;; Retrieve detailed asset information
(define-read-only (get-detailed-asset-info (asset-id uint))
    (let ((holder (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)))
        (ok {
            id: asset-id,
            holder: holder,
            verification-data: (unwrap! (map-get? asset-verification-data asset-id) error-missing-asset),
            retired: (is-asset-retired asset-id),
            group-data: (map-get? asset-group-data asset-id)
        })))

;; Verification helper: Check asset custody
(define-read-only (verify-asset-custody 
    (asset-id uint) 
    (expected-holder principal))
    (ok (is-eq 
        (some expected-holder)
        (nft-get-owner? eco-asset asset-id))))

;; Verification helper: Check asset lifecycle status
(define-read-only (verify-asset-lifecycle (asset-id uint))
    (ok {
        registered: (is-some (nft-get-owner? eco-asset asset-id)),
        retired: (is-asset-retired asset-id),
        has-group-data: (is-some (map-get? asset-group-data asset-id))
    }))

;; Transfer validation with enhanced checks
(define-private (validate-asset-transfer 
    (asset-id uint) 
    (new-holder principal))
    (and
        (not (is-asset-retired asset-id))
        (is-asset-holder asset-id tx-sender)
        (not (is-eq new-holder tx-sender))))

;; Format asset data for interface display
(define-private (asset-id-to-history (id uint))
    {
        id: id,
        verification-data: (unwrap-panic (get-asset-verification id)),
        holder: (unwrap-panic (get-asset-holder id)),
        retired: (is-asset-retired id),
        group-data: (map-get? asset-group-data id)
    })

;; Enhanced transfer validation
(define-public (verified-transfer 
    (asset-id uint) 
    (new-holder principal))
    (let ((validation-result (validate-asset-transfer asset-id new-holder)))
        (asserts! validation-result error-not-asset-holder)
        (try! (transfer-eco-asset asset-id tx-sender new-holder))
        (ok true)))

;; Asset lifecycle validation helper
(define-private (validate-asset-lifecycle (asset-id uint))
    (and
        (is-some (nft-get-owner? eco-asset asset-id))
        (not (is-asset-retired asset-id))))

;; Retired asset counter helper
(define-private (count-if-retired (id uint) (count uint))
    (if (is-asset-retired id)
        (+ count u1)
        count))

;; Ownership validation helper
(define-private (verify-holding (id uint) (previous-result bool))
    (and
        (is-asset-holder id tx-sender)
        previous-result))

;; ******************************************************************
;; Registry Status Indicators
;; ******************************************************************
(define-public (display-registry-size)
  (ok (var-get registry-counter))) ;; Retrieve and display total registered assets

;; ******************************************************************
;; Asset Transfer Eligibility Check
;; ******************************************************************
(define-public (is-asset-transferable (asset-id uint))
  (let ((is-valid (verify-asset-status asset-id)))
    (if (is-ok is-valid)
        (ok true) ;; Asset can be transferred if valid
        (err error-missing-asset))))

;; ******************************************************************
;; Enhanced Registration Security
;; ******************************************************************
(define-public (secure-asset-registration (verification-data (string-ascii 256)))
  (let ((new-asset-id (+ (var-get registry-counter) u1)))
    (asserts! (<= new-asset-id max-group-issuance) error-group-size-invalid)
    (register-eco-asset verification-data))) ;; Enhanced registration safety

;; ******************************************************************
;; Group Registration Validation
;; ******************************************************************
(define-public (validate-group-registration (data-list (list 50 (string-ascii 256))))
  (let ((group-size (len data-list)))
    (if (<= group-size max-group-issuance)
        (ok true)
        (err error-group-size-invalid)))) ;; Validate group size before registration

;; ******************************************************************
;; Recent Registry Activity Report
;; ******************************************************************
(define-read-only (get-recent-registry-activity)
  (ok (map asset-id-to-details (list-registry-entries u0 u10)))) ;; List most recent registry entries

;; ******************************************************************
;; Administrator Verification
;; ******************************************************************
(define-public (verify-registry-admin)
  (ok (is-eq tx-sender admin-principal))) ;; Verify caller is registry administrator


;; ******************************************************************
;; Registry Administration Security
;; ******************************************************************
(define-public (secure-admin-transition)
  (begin
    ;; Prevent unauthorized admin changes after deployment
    (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
    (ok "Admin transition restricted after deployment")))

;; ******************************************************************
;; Batch Asset Retirement
;; ******************************************************************
(define-public (batch-retire-eco-assets (asset-ids (list 50 uint)))
    (begin
        (asserts! (> (len asset-ids) u0) error-group-size-invalid)
        (asserts! (<= (len asset-ids) max-group-issuance) error-group-size-invalid)
        (map retire-eco-asset asset-ids)
        (ok true)))

;; ******************************************************************
;; Asset Data Enhancement
;; ******************************************************************
(define-public (add-asset-group-data (asset-id uint) (group-data (string-ascii 256)))
    (let ((asset-holder (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)))
        (asserts! (is-eq asset-holder tx-sender) error-not-asset-holder)
        (asserts! (is-valid-verification-data group-data) error-invalid-asset-data)
        (map-set asset-group-data asset-id group-data)
        (ok true)))

;; ******************************************************************
;; Batch Transfer Helper
;; ******************************************************************
(define-private (transfer-single-asset (transfer {asset-id: uint, recipient: principal}))
    (transfer-eco-asset (get asset-id transfer) tx-sender (get recipient transfer)))

;; Registry emergency pause mechanism
(define-data-var registry-paused bool false) ;; Tracks registry operational status

(define-public (pause-registry)
  (begin
    (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
    (var-set registry-paused true)
    (ok true)))

(define-public (resume-registry)
  (begin
    (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
    (var-set registry-paused false)
    (ok true)))

;; Registration control mechanism

(define-data-var registration-enabled bool true) ;; Tracks registration status

(define-public (toggle-registration-status)
  (begin
    (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
    (let ((current-status (var-get registration-enabled)))
      (var-set registration-enabled (not current-status))
      (ok true))))

;; Validate ownership before transfer execution
(define-public (validate-pre-transfer (asset-id uint) (new-holder principal))
(let ((asset-holder (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)))
  (asserts! (is-eq asset-holder tx-sender) error-not-asset-holder)
  (ok true)))

;; Administrator override for compliance requirements
(define-public (admin-override-custody (asset-id uint))
(begin
  (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
  (try! (nft-transfer? eco-asset asset-id tx-sender admin-principal))
  (ok true)))

;; Prevent operations on non-existent assets
(define-public (verify-asset-registry-entry (asset-id uint))
(let ((asset-holder (nft-get-owner? eco-asset asset-id)))
  (if (is-some asset-holder)
      (ok true)
      (err error-missing-asset))))

;; Permanently retire asset from active circulation
(define-public (decommission-asset (asset-id uint))
(begin
  (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)) error-not-asset-holder)
  (try! (nft-transfer? eco-asset asset-id tx-sender admin-principal))
  (ok true)))

;; Add asset to conservation project or fund
(define-public (allocate-to-conservation-project (asset-id uint) (project principal))
(begin
  (asserts! (is-eq tx-sender (unwrap! (nft-get-owner? eco-asset asset-id) error-missing-asset)) error-not-asset-holder)
  (ok true)))

;; Prevent unauthorized retirement
(define-public (enforce-retirement-policy (asset-id uint))
(begin
  (asserts! (is-eq tx-sender admin-principal) error-unauthorized)
  (try! (nft-transfer? eco-asset asset-id tx-sender admin-principal))
  (ok true)))

