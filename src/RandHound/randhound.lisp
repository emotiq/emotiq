;;; randhound.lisp

(defclass sid-mixin ()
  ((sid :initarg :sid :initform nil :accessor sid
        :type (array (unsigned byte 8))
        :documentation "Session identifier")))

(defclass sig-mixin ()
  ((sig :initarg :sig :initform nil :accessor sig
        :type (array (unsigned byte 8))
        :documentation "Schnorr signature")))

;; RandHound is the main protocol struct and implements the
;; onet.ProtocolInstance interface.

(defclass randhound (sid-mixin)
  ((treenode :initarg :treenode :initform nil :accessor treenode)
   (mutex :initarg :mutex :initform nil :accessor mutex)
   (nodes :initarg :nodes :initform nil :accessor nodes
          :documentation "Total number of nodes (client + servers)")
   (groups :initarg :groups :initform nil :accessor groups
           :documentation "Number of groups")
   (faulty :initarg :faulty :initform nil :accessor faulty
           :documentation "Maximum number of Byzantine servers")
   (purpose :initarg :purpose :initform nil :accessor purpose
            :documentation "Purpose of protocol run")
   (timestamp :initarg :timestamp :initform nil :accessor timestamp
              :documentation "Timestamp of initiation")
   (clirand :initarg :clirand :initform nil :accessor clirand
            :documentation "Client-chosen randomness (for initial sharding)")
   (server :initarg :server :initform nil :accessor server
           :documentation "Grouped servers")
   (group :initarg :group :initform nil :accessor group
          :documentation "Grouped server indices")
   (threshold :initarg :threshold :initform nil :accessor threshold
              :documentation "Group thresholds")
   (key :initarg :key :initform nil :accessor key
        :documentation "Grouped server public keys")
   (serveridxtogroupnum :initarg :serveridxtogroupnum :initform nil :accessor
                        serveridxtogroupnum
                        :documentation "Mapping of gloabl server index to group number")
   (serveridxtogroupidx :initarg :serveridxtogroupidx :initform nil :accessor
                        serveridxtogroupidx
                        :documentation "Mapping of global server index to group server index")
   (i1s :initarg :i1s :initform nil :accessor i1s
        :documentation "I1 messages sent to servers (index: group)")
   (i2s :initarg :i2s :initform nil :accessor i2s
        :documentation "I2 messages sent to servers (index: server)")
   (r1s :initarg :r1s :initform nil :accessor r1s
        :documentation "R1 messages received from servers (index: server)")
   (r2s :initarg :r2s :initform nil :accessor r2s
        :documentation "R2 messages received from servers (index: server)")
   (polycommit :initarg :polycommit :initform nil :accessor polycommit
               :documentation "Commitments of server polynomials (index: server)")
   (secret :initarg :secret :initform nil :accessor secret
           :documentation "Valid shares per secret/server (source server index -> list of target server indices)")
   (chosensecret :initarg :chosensecret :initform nil :accessor chosensecret
                 :documentation "Chosen secrets contributing to collective randomness")
   (done :initarg :done :initform nil :accessor done
         :documentation "Channel to signal the end of a protocol run")
   (secretready :initarg :secretready :initform nil :accessor secretready
                :documentation "Boolean to indicate whether the collect randomness is ready or not")))

#|
type RandHound struct {
	*onet.TreeNodeInstance

	mutex sync.Mutex

	// Session information
	nodes   int       // Total number of nodes (client + servers)
	groups  int       // Number of groups
	faulty  int       // Maximum number of Byzantine servers
	purpose string    // Purpose of protocol run
	time    time.Time // Timestamp of initiation
	cliRand []byte    // Client-chosen randomness (for initial sharding)
	sid     []byte    // Session identifier

	// Group information
	server              [][]*onet.TreeNode // Grouped servers
	group               [][]int            // Grouped server indices
	threshold           []int              // Group thresholds
	key                 [][]kyber.Point    // Grouped server public keys
	ServerIdxToGroupNum []int              // Mapping of gloabl server index to group number
	ServerIdxToGroupIdx []int              // Mapping of global server index to group server index

	// Message information
	i1s          map[int]*I1           // I1 messages sent to servers (index: group)
	i2s          map[int]*I2           // I2 messages sent to servers (index: server)
	r1s          map[int]*R1           // R1 messages received from servers (index: server)
	r2s          map[int]*R2           // R2 messages received from servers (index: server)
	polyCommit   map[int][]kyber.Point // Commitments of server polynomials (index: server)
	secret       map[int][]int         // Valid shares per secret/server (source server index -> list of target server indices)
	chosenSecret map[int][]int         // Chosen secrets contributing to collective randomness

	// Misc
	Done        chan bool // Channel to signal the end of a protocol run
	SecretReady bool      // Boolean to indicate whether the collect randomness is ready or not

	//Byzantine map[int]int // for simulating byzantine servers (= key)
}
|#

(defclass share ()
  ((source :initarg :source :initform nil :accessor source
           :type fixnum
           :documentation "Source server index")
   (target :initarg :target :initform nil :accessor target
           :type fixnum
           :documentation "Target server index")
   (pos :initarg :pos :initform nil :accessor pos
        :type fixnum
        :documentation "Share position")
   (val :initarg :val :initform nil :accessor val
        :documentation "Share value")
   (proof :initarg :proof :initform nil :accessor proof
          :type proofcore
          :documentation "ZK-verification proof"))
  (:documentation "Share encapsulates all information for encrypted or decrypted shares and the
          respective consistency proofs"))

(defclass i1 (sid-mixin sig-mixin)
  ((threshold :initarg :threshold :initform nil :accessor threshold
              :type fixnum
              :documentation "Secret sharing threshold")
   (group :initarg :group :initform nil :accessor group
          :type (array fixnum)
          :documentation "Group indices")
   (keys :initarg :keys :initform nil :accessor keys
         :type (array pubkey)
         :documentation "Public keys of trustees"))
  (:documentation "I1 is the message sent by the client to the servers in step 1"))

(defclass r1 (sig-mixin)
  ((hi1 :initarg :hi1 :initform nil :accessor hi1
        :type (array (unsigned byte 8))
        :documentation "Hash of I1")
   (encshare :initarg :encshare :initform nil :accessor encshare
             :type (array share)
             :documentation "Encrypted shares")
   (commitpoly :initarg :commitpoly :initform nil :accessor commitpoly
               :type (array (unsigned byte 8))
               :documentation "Marshalled commitment polynomial"))
  (:documentation "r1 is the reply sent by the servers to the client in step 2"))

(defclass i2 (sid-mixin sig-mixin)
  ((chosensecret :initarg :chosensecret :initform nil :accessor chosensecret
                 :type (array (unsigned byte 32))
                 :documentation "Chosen secrets (flattened)")
   (encshare :initarg :encshare :initform nil :accessor encshare
             :type (array share)
             :documentation "Encrypted shares")  
   (polycommit :initarg :polycommit :initform nil :accessor polycommit
               :documentation "Polynomial commitments"))
  (:documentation "I2 is the message sent by the client to the servers in step 3"))

(defclass r2 (sig-mixin)
  ((hi2 :initarg :hi2 :initform nil :accessor hi2
        :type (array (unsigned byte 8))
        :documentation "Hash of I2")
   (decshare :initarg :decshare :initform nil :accessor decshare
             :type (array share)
             :documentation "Decrypted shares"))
  (:documentation "R2 is the reply sent by the servers to the client in step 4"))

(defclass transcript (sid-mixin)
  ((nodes :initarg :nodes :initform nil :accessor nodes
          :type fixnum
          :documentation "Total number of nodes (client + server)")
   (groups :initarg :groups :initform nil :accessor groups
           :type fixnum
           :documentation "Number of groups")
   (faulty :initarg :faulty :initform nil :accessor faulty
           :type fixnum
           :documentation "Maximum number of Byzantine servers")
   (purpose :initarg :purpose :initform nil :accessor purpose
            :type string
            :documentation "Purpose of protocol run")
   (timestamp :initarg :timestamp :initform nil :accessor timestamp
         :type integer
         :documentation "Timestamp of initiation")
   (clirand :initarg :clirand :initform nil :accessor clirand
            :type (array (unsigned byte 8))
            :documentation "Client-chosen randomness (for initial sharding)")
   (clikey :initarg :clikey :initform nil :accessor clikey
           :type pubkey
           :documentation "Client public key")
   (group :initarg :group :initform nil :accessor group
          :type (array fixnum)
          :documentation "Grouped server indices")
   (key :initarg :key :initform nil :accessor key
        :type (array pubkey)
        :documentation "Grouped server public keys")
   (threshold :initarg :threshold :initform nil :accessor threshold)
   (chosensecret :initarg :chosensecret :initform nil :accessor chosensecret
                 :type hash-table
                 :documentation "Chosen secrets that contribute to collective randomness")
   (i1s :initarg :i1s :initform nil :accessor i1s
        :type hash-table
        :documentation "I1 messages sent to servers")
   (i2s :initarg :i2s :initform nil :accessor i2s
        :type hash-table
        :documentation "I2 messages sent to servers")
   (r1s :initarg :r1s :initform nil :accessor r1s
        :type hash-table
        :documentation "R1 messages received from servers")
   (r2s :initarg :r2s :initform nil :accessor r2s
        :type hash-table
        :documentation "R2 messages received from servers"))
  (:documentation "Transcript represents the record of a protocol run created by the client"))

; onet setup for randhound
(defun setup (nodes faulty groups purpose)
  (let ((rh (make-instance 'randhound
              :nodes nodes
              :faulty faulty
              :groups groups
              :purpose purpose)))
    rh
    ))

#|
a) Set the values in C and choose a random integer rT ∈R Zq 
as a seed to pseudorandomly create a balanced grouping T of S. Record C in L.
b) Prepare the message
⟨I1⟩x0 = ⟨H(C), T, u, w⟩x0 ,
record it in L, and broadcast it to all servers.
|#
(defmethod init ((rh randhound))
  (setf (timestamp rh) (get-universal-time))
  (setf (clirand rh) nil) ;[some random number from Zq]
  ; pseudorandomly create a balanced grouping T of S
  ; Record C in L.
  (let ((transcript (make-instance 'transcript
                      :sid (sid rh)
                      :nodes (nodes rh)
                      :groups (groups rh)
                      :faulty (faulty rh)
                      :purpose (purpose rh)
                      :timestamp (timestamp rh)
                      :clirand (clirand rh)
                      :clikey ; (public() rh) 
                      :group (group rh)
                      :threshold (threshold rh)
                      :chosensecret (chosensecret rh)
                      :key (key rh)
                      :i1s (i1s rh)
                      :i2s (i2s rh)
                      :r1s (r1s rh)
                      :r2s (r2s rh))))
    transcript
    ))