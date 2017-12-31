
(defun get-xlen ()
  "Get the length of the data registers in number of bits."
  16)

(defun get-instr-mem-load (instr-mem)
  "Get the instruction load function from an instruction info structure."
  (car instr-mem))

(defun get-instr-mem-struct (instr-mem)
  "Get the container of instructions from an instruction info structrue."
  (nth 1 instr-mem))

(defun get-pc (system-state)
  "Get the Program Counter from the system state structure."
  (car (nth 1 system-state)))

(defun incr-pc (system-state &optional val)
  "Increments the Program Counter from the system state structure."
  (if val
      (setcar (cdr system-state) (list (+ (get-pc system-state) val)))
    (setcar (cdr system-state) (list (+ (get-pc system-state) 4)))))

(defun get-data-reg (system-state idx)
  "Returns the current value stored in the data register at the given index."
  (car (aref (car system-state) idx)))

(defun set-data-reg (system-state idx val)
  "Sets the data register at the given index to the given value."
  (aset (car system-state) idx (cons val (aref (car system-state) idx))))

(defun get-current-instr (system-state instr-mem)
  "Returns the instruction referred to by the Program Counter."
  (funcall (get-instr-mem-load instr-mem) (get-pc system-state) (get-instr-mem-struct instr-mem)))

(defun get-csr (system-state addr)
  "Returns the CSR at the specified address."
  (if (gethash addr (nth 2 system-state))
      (gethash addr (nth 2 system-state))
    (puthash addr 0 (nth 2 system-state))))

(defun set-csr (system-state addr val)
  "Sets the CSR at the specified address to the given value."
  (puthash addr val (nth 2 system-state)))

(defun simulate (system-state instr-mem data-mem)
  "Simulates a RISC-V system."
  (catch 'ECALL
    (catch 'EBREAK
      (while (get-current-instr system-state instr-mem)
	(let ((current-instr (get-current-instr system-state instr-mem)))
	  (funcall (car current-instr) (cdr current-instr) system-state))
	(incr-pc system-state)))))


(defun ADD-base (system-state dest op1 op2)
  (set-data-reg system-state dest (+ op1 op2)))

(defun SLT-base (system-state dest op1 op2)
  (set-data-reg system-state dest (if (< op1 op2) 1 0)))

(defun AND-base (system-state dest op1 op2)
  (set-data-reg system-state dest (logand op1 op2)))

(defun OR-base (system-state dest op1 op2)
  (set-data-reg system-state dest (logior op1 op2)))

(defun XOR-base (system-state dest op1 op2)
  (set-data-reg system-state dest (logxor op1 op2)))

(defun SLL-base (system-state dest op1 op2)
  (set-data-reg system-state dest (lsh op1 op2)))

(defun SRL-base (system-state dest op1 op2)
  (set-data-reg system-state dest (lsh op1 (* op2 -1))))

(defun SRA-base (system-state dest op1 op2)
  (set-data-reg system-state dest (logior (lsh op1 (- op2))
					  (if (/= 0 (logand op1 (lsh (lognot 0) (- (get-xlen) 1))))
					      (logand (lognot (lsh (lognot 0) (get-xlen)))
						      (lsh (lognot 0) (- (get-xlen) op2)))
					    0))))


(defun OP-IMM (args system-state)
  "Implementation for arithmetic instruction with immediate value."
  (apply (car args) system-state (cdr args)))

(defun ADDI (system-state dest src imm)
  "Implementation for addition instruction with immediate value."
  (ADD-base system-state dest (get-data-reg system-state src) imm))

(defun SLTI (system-state dest src imm)
  "Implementation for set-less-than-immediate."
  (SLT-base system-state dest (get-data-reg system-state src) imm))

(defun ANDI (system-state dest src imm)
  "Implementation for AND instruction with immediate value."
  (AND-base system-state dest (get-data-reg system-state src) imm))

(defun ORI (system-state dest src imm)
  "Implementation for OR instruction with immediate value."
  (OR-base system-state dest (get-data-reg system-state src) imm))

(defun XORI (system-state dest src imm)
  "Implementation for XOR instruction with immediate value."
  (XOR-base system-state dest (get-data-reg system-state src) imm))

(defun SLLI (system-state dest src imm)
  "Implementation for shift-left-logical with immediate value."
  (SLL-base system-state dest (get-data-reg system-state src) imm))

(defun SRLI (system-state dest src imm)
  "Implementation for shift-right-logical with immediate value."
  (SRL-base system-state dest (get-data-reg system-state src) imm))

(defun SRAI (system-state dest src imm)
  "Implementation for shift-right-arithmetic with immediate value."
  (SRA-base system-state dest (get-data-reg system-state src) imm))


(defun LUI (args system-state)
  "Implementation for load-upper with immediate value."
  (set-data-reg system-state (car args) (nth 1 args)))

(defun AUIPC (args system-state)
  "Implementation for add-upper-to-pc with immediate value."
  (set-data-reg system-state (car args) (+ (nth 1 args) (get-pc system-state))))


(defun OP (args system-state)
  "Implementation for arithmetic instructions with register operands."
  (apply (car args) system-state (cdr args)))

(defun ADD (system-state dest src1 src2)
  "Implementation for addition with register operands."
  (ADD-base system-state dest (get-data-reg system-state src1) (get-data-reg system-state src2)))

(defun SLT (system-state dest src1 src2)
  "Implementation of Set-Less-Than with register operands."
  (SLT-base system-state dest (get-data-reg system-state src1) (get-data-reg system-state src2)))

(defun AND (system-state dest src1 src2)
  "Implementation of AND with register operands."
  (AND-base system-state dest (get-data-reg system-state src1) (get-data-reg system-state src2)))

(defun OR (system-state dest src1 src2)
  "Implementation of OR with register operands."
  (OR-base system-state dest (get-data-reg system-state src1) (get-data-reg system-state src2)))

(defun XOR (system-state dest src1 src2)
  "Implementation of XOR with register operands."
  (XOR-base system-state dest (get-data-reg system-state src1) (get-data-reg system-state src2)))

(defun SLL (system-state dest src1 src2)
  "Implementation of shift-left-logical with register operands."
  (SLL-base system-state dest (get-data-reg system-state src1) (get-data-reg system-state src2)))

(defun SRL (system-state dest src1 src2)
  "Implementation of shift-right-logical with register operands."
  (SRL-base system-state dest (get-data-reg system-state src1) (get-data-reg system-state src2)))

(defun SRA (system-state dest src1 src2)
  "Implementation of shift-right-arithmetic with register operands."
  (SRA-base system-state dest (get-data-reg system-state src1) (get-data-reg system-state src2)))

(defun SUB (system-state dest src1 src2)
  "Implementation of subtraction with register operands."
  (set-data-reg system-state dest (- (get-data-reg system-state src1) (get-data-reg system-state src2))))


(defun JAL (args system-state)
  "Implementation of Jump and Link instruction."
  (incr-pc system-state (nth 1 args))
  (set-data-reg system-state (car args) (+ (get-pc system-state) 4)))

(defun JALR (args system-state)
  "Implementation of Jump and Link Relative instruction."
  (let ((dest-reg (car args))
	(base-reg (nth 1 args))
	(offset (nth 2 args)))
    (incr-pc system-state (+ offset (get-data-reg system-state base-reg)))
    (set-data-reg system-state (car args) (+ (get-pc system-state) 4))))

(defun BRANCH (args system-state)
  "Implementation for conditional branching instructions."
  (apply (car args) system-state (cdr args)))

(defun BEQ (system-state src1 src2 offset)
  "Implementation for Branch if Equal instruction."
  (if (equal (get-data-reg system-state src1) (get-data-reg system-state src2))
      (incr-pc system-state offset)))

(defun BNE (system-state src1 src2 offset)
  "Implementation for Branch if Not Equal instruction."
  (if (not (equal (get-data-reg system-state src1) (get-data-reg system-state src2)))
      (incr-pc system-state offset)))

(defun BLT (system-state src1 src2 offset)
  "Implementation for Branch if Less Than instruction."
  (if (< (get-data-reg system-state src1) (get-data-reg system-state src2))
      (incr-pc system-state offset)))

(defun BGE (system-state src1 src2 offset)
  "Implementation for Branch if Greater Than or Equal instruction."
  (if (>= (get-data-reg system-state src1) (get-data-reg system-state src2))
      (incr-pc system-state offset)))


(defun SYSTEM (args system-state)
  "Implementation for control, status, and debugging instructions."
  (apply (car args) system-state (cdr args)))

(defun PRIV (system-state fun)
  "Implementation for priviledged instructions."
  (funcall fun))

(defun ECALL ()
  "Implementation of ECALL instruction."
  (throw 'ECALL t))

(defun EBREAK ()
  "Implementation of EBREAK instruction."
  (throw 'EBREAK t))

(defun CSRRW (system-state dest src addr)
  "Implementation of CSR Read-Write instruction."
  (set-data-reg system-state dest (get-csr system-state addr))
  (set-csr system-state addr (get-data-reg system-state src)))

(defun CSRRWI (system-state dest src addr)
  "Implementation of CSR Read-Write with Immediate instruction."
  (set-data-reg system-state dest (get-csr system-state addr))
  (set-csr system-state addr src))

(defun CSRRS (system-state dest src addr)
  "Implementation of CSR Read-Set instruction."
  (set-csr system-state addr (logior (get-csr system-state addr) (get-data-reg system-state src)))
  (set-data-reg system-state dest (get-csr system-state addr)))

(defun CSRRSI (system-state dest src addr)
  "Implementation of CSR Read-Set with Immediate instruction."
  (set-csr system-state addr (logior (get-csr system-state addr) src))
  (set-data-reg system-state dest (get-csr system-state addr)))

(defun CSRRC (system-state dest src addr)
  "Implementation of CSR Read-Clear instruction."
  (set-csr system-state addr (logand (get-csr system-state addr) (lognot (get-data-reg system-state src))))
  (set-data-reg system-state dest (get-csr system-state addr)))

(defun CSRRCI (system-state dest src addr)
  "Implementation of CSR Read-Clear with Immediate instruction."
  (set-csr system-state addr (logand (get-csr system-state addr) (lognot src)))
  (set-data-reg system-state dest (get-csr system-state addr)))


(defun read-buffer-instructions (instr-buffer)
  "Reads the instructions from the given buffer and returns a function list."
  (let ((instr-hash (init-instr-hash))
	(instrs ()))
    (save-excursion
      (set-buffer instr-buffer)
      (goto-char 0)
      (while (forward-word)
	(if (gethash (current-word) instr-hash)
	    (setq instrs (cons (funcall (gethash (current-word) instr-hash)) instrs)))))
    (nreverse instrs)))

(defun init-instr-hash ()
  "Creates and initializes an instruction string hash table."
  (define-hash-table-test 'str-test 'string= 'sxhash)
  (let ((instr-hash (make-hash-table :test 'str-test)))
    (puthash "addi" 'read-addi instr-hash)
    (puthash "slti" 'read-slti instr-hash)
    (puthash "andi" 'read-andi instr-hash)
    (puthash "ori" 'read-ori instr-hash)
    (puthash "xori" 'read-xori instr-hash)
    (puthash "slli" 'read-slli instr-hash)
    (puthash "srli" 'read-srli instr-hash)
    (puthash "srai" 'read-srai instr-hash)
    (puthash "lui" 'read-lui instr-hash)
    (puthash "auipc" 'read-auipc instr-hash)
    (puthash "jal" 'read-jal instr-hash)
    (puthash "jalr" 'read-jalr instr-hash)
    (puthash "beq" 'read-beq instr-hash)
    (puthash "bne" 'read-bne instr-hash)
    (puthash "blt" 'read-blt instr-hash)
    (puthash "bge" 'read-bge instr-hash)
    (puthash "ecall" 'read-ecall instr-hash)
    (puthash "ebreak" 'read-ebreak instr-hash)
    (puthash "csrrw" 'read-csrrw instr-hash)
    (puthash "csrrwi" 'read-csrrwi instr-hash)
    (puthash "csrrs" 'read-csrrs instr-hash)
    (puthash "csrrsi" 'read-csrrsi instr-hash)
    (puthash "csrrc" 'read-csrrc instr-hash)
    (puthash "csrrci" 'read-csrrci instr-hash)
    (puthash "rdcycle" 'read-rdcycle instr-hash)
    (puthash "rdtime" 'read-rdtime instr-hash)
    (puthash "rdinstret" 'read-rdinstret instr-hash)
    instr-hash))
;;  #s(hash-table test 'str-test data ("addi" 'read-addi)))

(defun read-register (word)
  "Parses the given word and returns an index of a data register."
  (string-to-number (substring word 1)))

(defun read-int-imm (func-type)
  "Reads an integer arithmetic with immediate instruction from the current buffer."
  (forward-word)
  (let ((op-type 'OP-IMM)
	(dest-reg 0)
	(src-reg 0)
	(imm 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq src-reg (read-register (current-word)))
    (forward-word)
    (setq imm (string-to-number (current-word)))
    (list op-type func-type dest-reg src-reg imm)))

(defun read-addi ()
  "Reads an ADDI instruction from the current buffer."
  (read-int-imm 'ADDI))

(defun read-slti ()
  "Reads a set-less-than-immediate instruction from the current buffer."
  (read-int-imm 'SLTI))

(defun read-andi ()
  "Reads an ANDI instruction from the current buffer."
  (read-int-imm 'ANDI))

(defun read-ori ()
  "Reads an ORI instruction from the current buffer."
  (read-int-imm 'ORI))

(defun read-xori ()
  "Reads an XORI instruction from the current buffer."
  (read-int-imm 'XORI))

(defun read-slli ()
  "Reads an SLLI instruction from the current buffer."
  (read-int-imm 'SLLI))

(defun read-srli ()
  "Reads an SRLI instruction from the current buffer."
  (read-int-imm 'SRLI))

(defun read-srai ()
  "Reads an SRAI instruction from the current buffer."
  (read-int-imm 'SRAI))

(defun read-lui ()
  "Reads an load-upper-immediate instruction from the current buffer."
  (forward-word)
  (let ((dest-reg 0)
	(offset 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq offset (string-to-number (current-word)))
    (list 'LUI dest-reg offset)))

(defun read-auipc ()
  "Reads an add-upper-immediate-to-pc instruction from the current buffer."
  (forward-word)
  (let ((dest-reg 0)
	(offset 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq offset (string-to-number (current-word)))
    (list 'AUIPC dest-reg offset)))

(defun read-jal ()
  "Read a Jump and Link instruction from the current buffer."
  (forward-word)
  (let ((op-type 'JAL)
	(dest-reg 0)
	(offset 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq offset (string-to-number (current-word)))
    (list op-type dest-reg offset)))

(defun read-jalr ()
  "Read a Jump and Link Relative instruction from the current buffer."
  (forward-word)
  (let ((op-type 'JALR)
	(dest-reg 0)
	(base-reg 0)
	(offset 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq base-reg (read-register (current-word)))
    (forward-word)
    (setq offset (string-to-number (current-word)))
    (list op-type dest-reg base-reg offset)))

(defun read-beq ()
  "Read a Branch if Equal instruction from the current buffer."
  (forward-word)
  (let ((op-type 'BRANCH)
	(func-type 'BEQ)
	(src1-reg 0)
	(src2-reg 0)
	(offset 0))
    (setq src1-reg (read-register (current-word)))
    (forward-word)
    (setq src2-reg (read-register (current-word)))
    (forward-word)
    (setq offset (string-to-number (current-word)))
    (list op-type func-type src1-reg src2-reg offset)))

(defun read-bne ()
  "Read a Branch if Not Equal instruction from the current buffer."
  (forward-word)
  (let ((op-type 'BRANCH)
	(func-type 'BNE)
	(src1-reg 0)
	(src2-reg 0)
	(offset 0))
    (setq src1-reg (read-register (current-word)))
    (forward-word)
    (setq src2-reg (read-register (current-word)))
    (forward-word)
    (setq offset (string-to-number (current-word)))
    (list op-type func-type src1-reg src2-reg offset)))

(defun read-blt ()
  "Read a Branch if Less Than instruction from the current buffer."
  (forward-word)
  (let ((op-type 'BRANCH)
	(func-type 'BLT)
	(src1-reg 0)
	(src2-reg 0)
	(offset 0))
    (setq src1-reg (read-register (current-word)))
    (forward-word)
    (setq src2-reg (read-register (current-word)))
    (forward-word)
    (setq offset (string-to-number (current-word)))
    (list op-type func-type src1-reg src2-reg offset)))

(defun read-bge ()
  "Read a Branch if Greater Than or Equal instruction from the current buffer."
  (forward-word)
  (let ((op-type 'BRANCH)
	(func-type 'BGE)
	(src1-reg 0)
	(src2-reg 0)
	(offset 0))
    (setq src1-reg (read-register (current-word)))
    (forward-word)
    (setq src2-reg (read-register (current-word)))
    (forward-word)
    (setq offset (string-to-number (current-word)))
    (list op-type func-type src1-reg src2-reg offset)))

(defun read-ecall ()
  "Reads an ECALL instruction from the current buffer."
  (forward-word)
  (list 'SYSTEM 'PRIV 'ECALL))

(defun read-ebreak ()
  "Reads an EBREAK instruction from the current buffer."
  (forward-word)
  (list 'SYSTEM 'PRIV 'EBREAK))

(defun read-csrrw ()
  "Reads a CSR Read-Write instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRW)
	(dest-reg 0)
	(src-reg 0)
	(csr-addr 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq src-reg (read-register (current-word)))
    (forward-word)
    (setq csr-addr (string-to-number (current-word)))
    (list op-type func-type dest-reg src-reg csr-addr)))

(defun read-csrrwi ()
  "Reads a CSR Read-Write with Immediate instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRWI)
	(dest-reg 0)
	(src-val 0)
	(csr-addr 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq src-val (string-to-number (current-word)))
    (forward-word)
    (setq csr-addr (string-to-number (current-word)))
    (list op-type func-type dest-reg src-val csr-addr)))

(defun read-csrrs ()
  "Reads a CSR Read-Set instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRS)
	(dest-reg 0)
	(src-reg 0)
	(csr-addr 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq src-reg (read-register (current-word)))
    (forward-word)
    (setq csr-addr (string-to-number (current-word)))
    (list op-type func-type dest-reg src-reg csr-addr)))

(defun read-csrrsi ()
  "Reads a CSR Read-Set with Immediate instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRSI)
	(dest-reg 0)
	(src-val 0)
	(csr-addr 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq src-val (string-to-number (current-word)))
    (forward-word)
    (setq csr-addr (string-to-number (current-word)))
    (list op-type func-type dest-reg src-val csr-addr)))

(defun read-csrrc ()
  "Reads a CSR Read-Clear instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRC)
	(dest-reg 0)
	(src-reg 0)
	(csr-addr 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq src-reg (read-register (current-word)))
    (forward-word)
    (setq csr-addr (string-to-number (current-word)))
    (list op-type func-type dest-reg src-reg csr-addr)))

(defun read-csrrci ()
  "Reads a CSR Read-Clear with Immediate instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRCI)
	(dest-reg 0)
	(src-val 0)
	(csr-addr 0))
    (setq dest-reg (read-register (current-word)))
    (forward-word)
    (setq src-val (string-to-number (current-word)))
    (forward-word)
    (setq csr-addr (string-to-number (current-word)))
    (list op-type func-type dest-reg src-val csr-addr)))

(defun read-rdcycle ()
  "Reads a Read Cycles instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRS)
	(dest-reg 0)
	(src-reg 0)
	(csr-addr 0))
    (setq dest-reg (read-register (current-word)))
    (list op-type func-type dest-reg src-reg csr-addr)))

(defun read-rdtime ()
  "Reads a Read Cycles instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRS)
	(dest-reg 0)
	(src-reg 0)
	(csr-addr 1))
    (setq dest-reg (read-register (current-word)))
    (list op-type func-type dest-reg src-reg csr-addr)))

(defun read-rdinstret ()
  "Reads a Read Instructions Retired instruction from the current buffer."
  (forward-word)
  (let ((op-type 'SYSTEM)
	(func-type 'CSRRS)
	(dest-reg 0)
	(src-reg 0)
	(csr-addr 2))
    (setq dest-reg (read-register (current-word)))
    (list op-type func-type dest-reg src-reg csr-addr)))


(defun make-zeroed-system-state ()
  "Initializes system state with all registers and memory locations set to zero."
  (list (make-vector 32 '(0)) '(0) (make-hash-table)))

(ert-deftest test-pc ()
  "Tests that the PC is incremented."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(OP-IMM ADDI 0 0 0) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-pc system-state)))))

(defun prepare-op (func-info system-state instr-mem-hash immp src2-val src2-loc func)
  (if immp
      (puthash 0 (append '(OP-IMM) (list func) func-info (list src2-val)) instr-mem-hash)
    (puthash 0 (append '(OP) (list func) func-info (list src2-loc)) instr-mem-hash)
    (set-data-reg system-state src2-loc src2-val)))

(defun test-add-base (immp func)
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 0) system-state instr-mem-hash immp 4 2 func)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(2 0) system-state instr-mem-hash immp 8 3 func)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-data-reg system-state 2)))))

(ert-deftest test-addi ()
  "Tests that ADDI works."
  (test-add-base t 'ADDI))

(ert-deftest test-add ()
  "Tests that ADD works."
  (test-add-base nil 'ADD))

(defun test-slt-base (immp func)
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 0) system-state instr-mem-hash immp 0 2 func)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 0 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 0) system-state instr-mem-hash immp 1 2 func)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 1 (get-data-reg system-state 1)))))

(ert-deftest test-slti ()
  "Tests that Set-Less-Than-Immediate works."
  (test-slt-base t 'SLTI))

(ert-deftest test-slt ()
  "Tests that Set-Less-Than with register operands works."
  (test-slt-base nil 'SLT))

(defun test-and-base (immp func)
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 0) system-state instr-mem-hash immp 0 2 func)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 0 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 4 2 func)
    (set-data-reg system-state 1 5)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-data-reg system-state 1)))))

(ert-deftest test-andi ()
  "Tests that ANDI works."
  (test-and-base t 'ANDI))

(ert-deftest test-and ()
  "Tests that AND works."
  (test-and-base nil 'AND))

(defun test-or-base (immp func)
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 0) system-state instr-mem-hash immp 0 2 func)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 0 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 0) system-state instr-mem-hash immp 1 2 func)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 1 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 3 2 func)
    (set-data-reg system-state 1 5)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 7 (get-data-reg system-state 1)))))
  
(ert-deftest test-ori ()
  "Tests that ORI works."
  (test-or-base t 'ORI))

(ert-deftest test-or ()
  "Tests that OR works."
  (test-or-base nil 'OR))

(defun test-xor-base (immp func)
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 3 2 func)
    (set-data-reg system-state 1 5)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 6 (get-data-reg system-state 1)))))

(ert-deftest test-xori ()
  "Tests that XORI works."
  (test-xor-base t 'XORI))

(ert-deftest test-xor ()
  "Tests that XOR works."
  (test-xor-base nil 'XOR))

(defun test-sll-base (immp func)
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 1 2 func)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 2 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 2 2 func)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 1 2 func)
    (set-data-reg system-state 1 2)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-data-reg system-state 1)))))

(ert-deftest test-slli ()
  "Tests that shift-left-logical-immediate works."
  (test-sll-base t 'SLLI))

(ert-deftest test-sll ()
  "Tests that shift-left-logical with register operands works."
  (test-sll-base nil 'SLL))

(defun test-srl-base (immp func)
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 1 2 func)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 0 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 1 2 func)
    (set-data-reg system-state 1 2)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 1 (get-data-reg system-state 1)))))

(ert-deftest test-srli ()
  "Tests that shift-right-logical-immediate works."
  (test-srl-base t 'SRLI))

(ert-deftest test-srl ()
  "Tests that shift-right-logical with register operands works."
  (test-srl-base nil 'SRL))

(defun test-sra-base (immp func)
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 1 2 func)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (= 0 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
  	(instr-mem-hash (make-hash-table))
  	(data-mem-hash (make-hash-table)))
    (prepare-op '(1 1) system-state instr-mem-hash immp 1 2 func)
    (set-data-reg system-state 1 (lsh 1 (- (get-xlen) 1)))
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (= (lsh 3 (- (get-xlen) 2)) (get-data-reg system-state 1)))))

(ert-deftest test-srai ()
  "Tests that shift-right-arithmetic-immediate works."
  (test-sra-base t 'SRAI))

(ert-deftest test-sra ()
  "Tests that shift-right-arithmetic with register operands works."
  (test-sra-base nil 'SRA))

(ert-deftest test-sub ()
  "Tests that subtraction with register operands."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(OP SUB 1 0 2) instr-mem-hash)
    (set-data-reg system-state 2 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (= -1 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(OP SUB 1 3 2) instr-mem-hash)
    (set-data-reg system-state 3 1)
    (set-data-reg system-state 2 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (= 0 (get-data-reg system-state 1)))))


(ert-deftest test-lui ()
  "Tests that load-upper-immediate works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 (list 'LUI 1 (lsh 1 12)) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (= (lsh 1 12) (get-data-reg system-state 1)))))

(ert-deftest test-auipc ()
  "Tests that add-upper-immediate-to-pc works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 (list 'AUIPC 1 (lsh 1 12)) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (= (lsh 1 12) (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(OP-IMM ADDI 0 0 0) instr-mem-hash)
    (puthash 4 (list 'AUIPC 1 (lsh 1 12)) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (= (+ 4 (lsh 1 12)) (get-data-reg system-state 1)))))

(ert-deftest test-ecall ()
  "Tests that ECALL works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(SYSTEM PRIV ECALL) instr-mem-hash)
    (puthash 4 '(OP-IMM ANDI 0 0 0) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 0 (get-pc system-state)))))

(ert-deftest test-ebreak ()
  "Tests that EBREAK works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (puthash 4 '(OP-IMM ANDI 0 0 0) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 0 (get-pc system-state)))))

(ert-deftest test-csrrw ()
  "Tests that CSR Read-Write works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(SYSTEM CSRRW 2 1 0) instr-mem-hash)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 1 (get-csr system-state 0)))))

(ert-deftest test-csrrs ()
  "Tests that CSR Read-Set works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(SYSTEM CSRRS 2 1 0) instr-mem-hash)
    (set-data-reg system-state 1 3)
    (set-csr system-state 0 5)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 7 (get-csr system-state 0)))
    (should (equal 7 (get-data-reg system-state 2)))))

(ert-deftest test-csrrc ()
  "Tests that CSR Read-Clear works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(SYSTEM CSRRC 2 1 0) instr-mem-hash)
    (set-data-reg system-state 1 3)
    (set-csr system-state 0 5)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-csr system-state 0)))
    (should (equal 4 (get-data-reg system-state 2)))))

(ert-deftest test-csrrwi ()
  "Tests that CSR Read-Write with Immediate works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(SYSTEM CSRRWI 2 1 0) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 1 (get-csr system-state 0)))))

(ert-deftest test-csrrsi ()
  "Tests that CSR Read-Set with Immediate works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(SYSTEM CSRRSI 2 3 0) instr-mem-hash)
    (set-csr system-state 0 5)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 7 (get-csr system-state 0)))
    (should (equal 7 (get-data-reg system-state 2)))))

(ert-deftest test-csrrci ()
  "Tests that CSR Read-Clear with Immediate works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(SYSTEM CSRRCI 2 3 0) instr-mem-hash)
    (set-csr system-state 0 5)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-csr system-state 0)))
    (should (equal 4 (get-data-reg system-state 2)))))

(ert-deftest test-jal ()
  "Tests that Jump and Link works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(JAL 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-pc system-state)))
    (should (equal 8 (get-data-reg system-state 1)))))

(ert-deftest test-jalr ()
  "Tests that Jump and Link works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(JALR 1 2 0) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (set-data-reg system-state 2 4)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-pc system-state)))
    (should (equal 8 (get-data-reg system-state 1)))))

(ert-deftest test-beq ()
  "Tests that Branch if Equal works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BEQ 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-pc system-state))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BEQ 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-pc system-state)))))

(ert-deftest test-bne ()
  "Tests that Branch if Not Equal works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BNE 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-pc system-state))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BNE 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-pc system-state)))))

(ert-deftest test-blt ()
  "Tests that Branch if Less Than works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BLT 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-pc system-state))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BLT 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (set-data-reg system-state 1 -1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-pc system-state))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BLT 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-pc system-state)))))

(ert-deftest test-bge ()
  "Tests that Branch if Greater Than or Equal works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BGE 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-pc system-state))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BGE 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (set-data-reg system-state 1 -1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-pc system-state))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(BRANCH BGE 0 1 4) instr-mem-hash)
    (puthash 4 '(SYSTEM PRIV EBREAK) instr-mem-hash)
    (set-data-reg system-state 1 1)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-pc system-state)))))

(defun read-tmp-instruction (instr)
  "Writes the given instruction string to a temporary buffer and returns the instruction symbols."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert instr))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    instrs))

(ert-deftest test-read-addi ()
  "Tests that an ADDI instruction can be read."
  (should (equal (list '(OP-IMM ADDI 1 0 0)) (read-tmp-instruction "addi x1, x0, 0")))
  (should (equal (list '(OP-IMM ADDI 1 0 1)) (read-tmp-instruction "addi x1, x0, 1")))
  (should (equal (list '(OP-IMM ADDI 22 15 256)) (read-tmp-instruction "addi x22, x15, 256"))))

(ert-deftest test-read-slti ()
  "Tests that a set-less-than-immediate instruction can be read."
  (should (equal (list '(OP-IMM SLTI 1 0 0)) (read-tmp-instruction "slti x1, x0, 0"))))

(ert-deftest test-read-andi ()
  "Tests that an ANDI instruction can be read."
  (should (equal (list '(OP-IMM ANDI 1 1 5)) (read-tmp-instruction "andi x1, x1, 5"))))

(ert-deftest test-read-ori ()
  "Tests that an ORI instruction can be read."
  (should (equal (list '(OP-IMM ORI 1 0 1)) (read-tmp-instruction "ori x1,x0,1"))))

(ert-deftest test-read-xori ()
  "Tests that an XORI instruction can be read."
  (should (equal (list '(OP-IMM XORI 1 0 2)) (read-tmp-instruction "xori x1,x0,2"))))

(ert-deftest test-read-slli ()
  "Tests that an SLLI instruction can be read."
  (should (equal (list '(OP-IMM SLLI 1 1 3)) (read-tmp-instruction "slli x1,x1,3"))))

(ert-deftest test-read-srli ()
  "Tests that an SRLI instruction can be read."
  (should (equal (list '(OP-IMM SRLI 3 4 2)) (read-tmp-instruction "srli x3,x4,2"))))

(ert-deftest test-read-srai ()
  "Tests that an SRAI instruction can be read."
  (should (equal (list '(OP-IMM SRAI 12 5 6)) (read-tmp-instruction "srai x12,x5,6"))))

(ert-deftest test-read-lui ()
  "Tests that a load-upper-immediate can be read."
  (should (equal (list (list 'LUI 1 (lsh 1 12))) (read-tmp-instruction "lui x1,4096"))))

(ert-deftest test-read-auipc ()
  "Tests that a add-upper-immediate-to-pc can be read."
  (should (equal (list (list 'AUIPC 1 (lsh 1 12))) (read-tmp-instruction "auipc x1,4096"))))

(ert-deftest test-read-ecall ()
  "Tests that an ECALL instruction can be read."
  (should (equal (list '(SYSTEM PRIV ECALL)) (read-tmp-instruction "ecall"))))

(ert-deftest test-read-ebreak ()
  "Tests that an EBREAK instruction can be read."
  (should (equal (list '(SYSTEM PRIV EBREAK)) (read-tmp-instruction "ebreak"))))

(ert-deftest test-read-csrrw ()
  "Tests that a CSR Read-Write instruction can be read."
  (should (equal (list '(SYSTEM CSRRW 2 1 0)) (read-tmp-instruction "csrrw x2, x1, 0"))))

(ert-deftest test-read-csrrwi ()
  "Tests that a CSR Read-Write with Immediate instruction can be read."
  (should (equal (list '(SYSTEM CSRRWI 2 1 3)) (read-tmp-instruction "csrrwi x2, 1, 3"))))

(ert-deftest test-read-csrrs ()
  "Tests that a CSR Read-Set instruction can be read."
  (should (equal (list '(SYSTEM CSRRS 2 1 4)) (read-tmp-instruction "csrrs x2, x1, 4"))))

(ert-deftest test-read-csrrsi ()
  "Tests that a CSR Read-Set with Immediate instruction can be read."
  (should (equal (list '(SYSTEM CSRRSI 2 1 4)) (read-tmp-instruction "csrrsi x2, 1, 4"))))

(ert-deftest test-read-csrrc ()
  "Tests that a CSR Read-Clear instruction can be read."
  (should (equal (list '(SYSTEM CSRRC 3 5 3)) (read-tmp-instruction "csrrc x3, x5, 3"))))

(ert-deftest test-read-csrrci ()
  "Tests that a CSR Read-Clear with Immediate instruction can be read."
  (should (equal (list '(SYSTEM CSRRCI 3 5 3)) (read-tmp-instruction "csrrci x3, 5, 3"))))

(ert-deftest test-read-rdcycle ()
  "Tests that a Read Cycles instruction can be read."
  (should (equal (list '(SYSTEM CSRRS 3 0 0)) (read-tmp-instruction "rdcycle x3"))))

(ert-deftest test-read-rdtime ()
  "Tests that a Read Time instruction can be read."
  (should (equal (list '(SYSTEM CSRRS 5 0 1)) (read-tmp-instruction "rdtime x5"))))

(ert-deftest test-read-rdinstret ()
  "Tests that a Read Instructions Retired instruction can be read."
  (should (equal (list '(SYSTEM CSRRS 2 0 2)) (read-tmp-instruction "rdinstret x2"))))

(ert-deftest test-read-jal ()
  "Tests that a Jump and Link instruction can be read."
  (should (equal (list '(JAL 4 8)) (read-tmp-instruction "jal x4, 8"))))

(ert-deftest test-read-jalr ()
  "Tests that a Jump and Link Relative instruction can be read."
  (should (equal (list '(JALR 4 7 16)) (read-tmp-instruction "jalr x4, x7, 16"))))

(ert-deftest test-read-beq ()
  "Tests that a Branch if Equal instruction can be read."
  (should (equal (list '(BRANCH BEQ 0 1 4)) (read-tmp-instruction "beq x0, x1, 4"))))

(ert-deftest test-read-bne ()
  "Tests that a Branch if Not Equal instruction can be read."
  (should (equal (list '(BRANCH BNE 3 4 8)) (read-tmp-instruction "bne x3, x4, 8"))))

(ert-deftest test-read-blt ()
  "Tests that a Branch if Less Than instruction can be read."
  (should (equal (list '(BRANCH BLT 6 12 8)) (read-tmp-instruction "blt x6, x12, 8"))))

(ert-deftest test-read-bge ()
  "Tests that a Branch if Greater Than or Equal instruction can be read."
  (should (equal (list '(BRANCH BGE 6 7 -4)) (read-tmp-instruction "bge x6, x7, -4"))))
