
(defun get-instr-mem-load (instr-mem)
  "Get the instruction load function from an instruction info structure."
  (car instr-mem))

(defun get-instr-mem-struct (instr-mem)
  "Get the container of instructions from an instruction info structrue."
  (nth 1 instr-mem))

(defun get-pc (system-state)
  "Get the Program Counter from the system state structure."
  (car (nth 1 system-state)))

(defun incr-pc (system-state)
  "Increments the Program Counter from the system state structure."
  (setcar (cdr system-state) (list (+ (get-pc system-state) 4))))

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


(defun OP-IMM (args system-state)
  "Implemenation for arithmetic instruction with immediate value."
  (apply (car args) system-state (cdr args)))

(defun ADDI (system-state dest src imm)
  "Implementation for addition instruction with immediate value."
  (set-data-reg system-state dest (+ (get-data-reg system-state src) imm)))

(defun ANDI (system-state dest src imm)
  "Implementation for AND instruction with immediate value."
  (set-data-reg system-state dest (logand (get-data-reg system-state src) imm)))


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
  "Implemenation of CSR Read-Set instruction."
  (set-csr system-state addr (logior (get-csr system-state addr) (get-data-reg system-state src)))
  (set-data-reg system-state dest (get-csr system-state addr)))

(defun CSRRSI (system-state dest src addr)
  "Implemenation of CSR Read-Set with Immediate instruction."
  (set-csr system-state addr (logior (get-csr system-state addr) src))
  (set-data-reg system-state dest (get-csr system-state addr)))

(defun CSRRC (system-state dest src addr)
  "Implemenation of CSR Read-Clear instruction."
  (set-csr system-state addr (logand (get-csr system-state addr) (lognot (get-data-reg system-state src))))
  (set-data-reg system-state dest (get-csr system-state addr)))

(defun CSRRCI (system-state dest src addr)
  "Implemenation of CSR Read-Clear with Immediate instruction."
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
    (puthash "andi" 'read-andi instr-hash)
    (puthash "ecall" 'read-ecall instr-hash)
    (puthash "ebreak" 'read-ebreak instr-hash)
    (puthash "csrrw" 'read-csrrw instr-hash)
    (puthash "csrrwi" 'read-csrrwi instr-hash)
    (puthash "csrrs" 'read-csrrs instr-hash)
    (puthash "csrrsi" 'read-csrrsi instr-hash)
    (puthash "csrrc" 'read-csrrc instr-hash)
    (puthash "csrrci" 'read-csrrci instr-hash)
    instr-hash))
;;  #s(hash-table test 'str-test data ("addi" 'read-addi)))

(defun read-addi ()
  "Reads an ADDI instruction from the current buffer."
  (forward-word)
  (let ((op-type 'OP-IMM)
	(func-type 'ADDI)
	(dest-reg 0)
	(src-reg 0)
	(imm 0))
    (setq dest-reg (string-to-number (substring (current-word) 1)))
    (forward-word)
    (setq src-reg (string-to-number (substring (current-word) 1)))
    (forward-word)
    (setq imm (string-to-number (current-word)))
    (list op-type func-type dest-reg src-reg imm)))

(defun read-andi ()
  "Reads an ANDI instruction from the current buffer."
  (forward-word)
  (let ((op-type 'OP-IMM)
	(func-type 'ANDI)
	(dest-reg 0)
	(src-reg 0)
	(imm 0))
    (setq dest-reg (string-to-number (substring (current-word) 1)))
    (forward-word)
    (setq src-reg (string-to-number (substring (current-word) 1)))
    (forward-word)
    (setq imm (string-to-number (current-word)))
    (list op-type func-type dest-reg src-reg imm)))

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
    (setq dest-reg (string-to-number (substring (current-word) 1)))
    (forward-word)
    (setq src-reg (string-to-number (substring (current-word) 1)))
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
    (setq dest-reg (string-to-number (substring (current-word) 1)))
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
    (setq dest-reg (string-to-number (substring (current-word) 1)))
    (forward-word)
    (setq src-reg (string-to-number (substring (current-word) 1)))
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
    (setq dest-reg (string-to-number (substring (current-word) 1)))
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
    (setq dest-reg (string-to-number (substring (current-word) 1)))
    (forward-word)
    (setq src-reg (string-to-number (substring (current-word) 1)))
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
    (setq dest-reg (string-to-number (substring (current-word) 1)))
    (forward-word)
    (setq src-val (string-to-number (current-word)))
    (forward-word)
    (setq csr-addr (string-to-number (current-word)))
    (list op-type func-type dest-reg src-val csr-addr)))


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

(ert-deftest test-addi ()
  "Tests that ADDI works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(OP-IMM ADDI 1 0 4) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(OP-IMM ADDI 2 0 8) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 8 (get-data-reg system-state 2)))))

(ert-deftest test-andi ()
  "Tests that ANDI works."
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(OP-IMM ANDI 1 0 0) instr-mem-hash)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 0 (get-data-reg system-state 1))))
  (let ((system-state (make-zeroed-system-state))
	(instr-mem-hash (make-hash-table))
	(data-mem-hash (make-hash-table)))
    (puthash 0 '(OP-IMM ANDI 1 1 4) instr-mem-hash)
    (set-data-reg system-state 1 5)
    (simulate system-state (list 'gethash instr-mem-hash) (list 'gethash 'puthash data-mem-hash))
    (should (equal 4 (get-data-reg system-state 1)))))

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


(ert-deftest test-read-addi ()
  "Tests that an ADDI instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "addi x1, x0, 0"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(OP-IMM ADDI 1 0 0)) instrs)))
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "addi x1, x0, 1"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(OP-IMM ADDI 1 0 1)) instrs)))
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "addi x22, x15, 256"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(OP-IMM ADDI 22 15 256)) instrs))))

(ert-deftest test-read-andi ()
  "Tests that an ANDI instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "andi x1, x1, 5"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(OP-IMM ANDI 1 1 5)) instrs))))

(ert-deftest test-read-ecall ()
  "Tests that an ECALL instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "ecall"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(SYSTEM PRIV ECALL)) instrs))))

(ert-deftest test-read-ebreak ()
  "Tests that an EBREAK instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "ebreak"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(SYSTEM PRIV EBREAK)) instrs))))

(ert-deftest test-read-csrrw ()
  "Tests that a CSR Read-Write instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "csrrw x2, x1, 0"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(SYSTEM CSRRW 2 1 0)) instrs))))

(ert-deftest test-read-csrrwi ()
  "Tests that a CSR Read-Write with Immediate instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "csrrwi x2, 1, 3"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(SYSTEM CSRRWI 2 1 3)) instrs))))

(ert-deftest test-read-csrrs ()
  "Tests that a CSR Read-Set instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "csrrs x2, x1, 4"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(SYSTEM CSRRS 2 1 4)) instrs))))

(ert-deftest test-read-csrrsi ()
  "Tests that a CSR Read-Set with Immediate instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "csrrsi x2, 1, 4"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(SYSTEM CSRRSI 2 1 4)) instrs))))

(ert-deftest test-read-csrrc ()
  "Tests that a CSR Read-Clear instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "csrrc x3, x5, 3"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(SYSTEM CSRRC 3 5 3)) instrs))))


(ert-deftest test-read-csrrci ()
  "Tests that a CSR Read-Clear with Immediate instruction can be read."
  (let ((instr-buf (get-buffer-create "*riscv-test*"))
	(instrs))
    (save-excursion
      (set-buffer instr-buf)
      (insert "csrrci x3, 5, 3"))
    (setq instrs (read-buffer-instructions instr-buf))
    (kill-buffer instr-buf)
    (should (equal (list '(SYSTEM CSRRCI 3 5 3)) instrs))))
