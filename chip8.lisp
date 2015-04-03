(ql:quickload 'lispbuilder-sdl)

;;Memory 4k mem
(defvar *memory* (make-array 4095))
;;fontset to be stored in mem should proably just store in mem at begin
;(defvar *fontset* (make-array 80))
;;Registors V0-VF
(defvar *v* (make-array 16))
;;holds the opcode
(defvar *opcode* 0)
;;Index Adress Register
(defvar *i* 0)
;;Program Counter
(defvar *pc* 0)
;;stack (for returning subroutine addresses)
(defvar *stack* (make-array 16))
;;stack pointer
(defvar *sp* 0)
;;Timers Delay Timer and Sound Timer
(defvar *dt* 0)
(defvar *st* 0)
;;the screen array
(defvar *gfx* (make-array (* 64 32)))
;;keyboard state
(defvar *keypad* (make-array 16))

(defun chipinit()
  (setf *pc* #x200)
  (setf *opcode* 0)
  (setf *i* 0)
  (setf *sp* 0)
  (loop for x in '(#xF0 #x90 #x90 #x90 #xF0 ;0
                   #x20 #x60 #x20 #x20 #x70 ;1
                   #xF0 #x10 #xF0 #x80 #xF0 ;2
                   #xF0 #x10 #xF0 #x10 #xF0 ;3
                   #x90 #x90 #xF0 #x10 #x10 ;4
                   #xF0 #x80 #xF0 #x10 #xF0 ;5
                   #xF0 #x80 #xF0 #x90 #xF0 ;6
                   #xF0 #x10 #x20 #x40 #x40 ;7
                   #xF0 #x90 #xF0 #x90 #xF0 ;8
                   #xF0 #x90 #xF0 #x10 #xF0 ;9
                   #xF0 #x90 #xF0 #x90 #x90 ;A
                   #xE0 #x90 #xE0 #x90 #xE0 ;B
                   #xF0 #x80 #x80 #x80 #xF0 ;C
                   #xE0 #x90 #x90 #x90 #xE0 ;D
                   #xF0 #x80 #xF0 #x80 #xF0 ;E
                   #xF0 #x80 #xF0 #x80 #x80 ;F
                   
                   )
        for i from 0 to 80 
        do (setf (aref *memory* i) x)))
        
(defun fetchop()
  (setf *opcode* (logior (ash (aref *memory* *pc*) 8) (aref *memory* (+ *pc* 1)))))
  
(defun decodeop()
  (case (logand *opcode* #xF000)
    (#x0000 (case (logand *opcode* #x00FF)
             (#x00E0 (string "Clear Screen")) ;;opcode 00E0 clear screen
             (#x00EE (string "return from sub")) ;;opcode 00EE return from subroutine
             (otherwise (string "Fucked up 0x0XXX opcode!"))))
	(#x1000 (string "Jump to address")) ;;1NNN
	(#x2000 (string "subroutine at address")) ;;2NNN
	(#x3000 (when (= (aref *v* (ash (logand *opcode* #x0F00) -8)) (logand *opcode* #x00FF))
            (setf *pc* (+ *pc* 2)))) ;;3XNN skip next instruction if VX == NN
	(#x4000 (when (not (= (aref *v* (ash (logand *opcode* #x0F00) -8)) (logand *opcode* #x00FF)))
            (setf *pc* (+ *pc* 2))))
    ;;string "skip if VX != NN")) ;;4XNN
	(#x5000 (when (= (aref *v* (ash (logand *opcode* #x0F00) -8)) (aref *v* (ash (logand *opcode* #x00F0) -4)))
            (setf *pc* (+ *pc* 2))));;skip the next instruction if VX == VY ;;5XY0
	(#x6000 (setf (aref *v* (ash (logand *opcode* #x0F00) -8)) (logand *opcode* #x00FF)));;string "set VX to NN")) ;;6XNN
	(#x7000 (setf (aref *v* (ash (logand *opcode* #x0F00) -8)) (+ (aref *v* (ash (logand *opcode* #x0F00) -8)) (logand *opcode* #x00FF))));;(string "add NN to VX")) ;;7XNN
	(#x8000 (case (logand *opcode* #x000F)
              (#x0000 (setf (aref *v* (ash (logand *opcode* #x0F00) -8)) (aref *v* (ash (logand *opcode* #x00F0) -4))));;(string "Set VX to VY")) ;;8XY0
              (#x0001 (setf (aref *v* (ash (logand *opcode* #x0F00) -8)) (logior (aref *v* (ash (logand *opcode* #x0F00) -8)) (aref *v* (ash (logand *opcode* #x00F0) -4))))) ;(string "Sets VX to VX or VY")) ;;8XY1
              (#x0002 (setf (aref *v* (ash (logand *opcode* #x0F00) -8)) (logand (aref *v* (ash (logand *opcode* #x0F00) -8)) (aref *v* (ash (logand *opcode* #x00F0) -4)))));;(string "Sets VX to VX and VY")) ;;8XY2
              (#x0003 (setf (aref *v* (ash (logand *opcode* #x0F00) -8)) (logxor (aref *v* (ash (logand *opcode* #x0F00) -8)) (aref *v* (ash (logand *opcode* #x00F0) -4)))));;(string "Sets VX to VX xor VY")) ;;8XY3
              (#x0004 (let ((sum 0)) (setq sum (+ (aref *v* (ash (logand *opcode* #x0F00) -8)) (aref *v* (ash (logand *opcode* #x00F0) -4))))
                      (setf (aref *v* (ash (logand *opcode* #x0F00) -8)) (logand sum #xFFFF))
                      (setf (aref *v* 15) (ash (logand sum #xF0000) -16))));;(string "Add VY to VX VF is carry")) ;;8XY4
              (#x0005 (string "VX - VY Vf is 0 when borrow")) ;;8XY5
              (#x0006 (string "Shift VX right by 1 VF is [0]")) ;;8XY6
              (#x0007 (string "VX = VY-VX  VF 0 when borrow")) ;;8XY7
              (#x000E (string "VX left shift by 1 VF is [15]")) ;;8XYE
              (otherwise (string "Fucked up 0x8XXX opcode!"))))
    (#x9000 (string "skip instruction if VX != VY")) ;;9XY0
    (#xA000 (string "Sets I to NNN")) ;; ANNN
    (#xB000 (string "jumps to adress NNN + V0")) ;;BNNN
    (#xC000 (string "sets vx to random number and NN"));;CXNN
    (#xD000 (string "Drawing or some shit")) ;;DXYN
    (#xE000 (case (logand *opcode* #x00FF)
              (#x009E (string "SKIP INSTRCUOTN IF KEY IN VX prESSD")) ;EX9E
              (#x00A1 (string "SKIP INSTRUCITON IF VX ISNT PRESED"));;EXA1
              (otherwise (string "FUCKED 0xE000 opcode!"))))
    (#xF000 (case (logand *opcode* #x00FF)
              (#x0007 (string "Sets VX to value of delay timer"))
              (#x000A (string "Key press awaited then stored in VX"))
              (#x0015 (string "Set delay timer to VX"))
              (#x0018 (string "Set sound timer to VX"))
              (#x001E (string "Add VX to I"))
              (#x0029 (string "Set I to location of char @ VX"))
              (#x0033 (string "Binary coded decimal of VX"))
              (#x0055 (string "Store V0 to VX starting at I"))
              (#x0065 (string "Fills V0 to VX with values of mem from I"))
              (otherwise (string "FUCKED 0xFXXX opcode!"))))
    (otherwise -1)))
    
;;define the main running function
(defun chip8 ()
  (sdl:with-init()
    (sdl:window 640 320 :bpp 16 :title-caption "CHIP8 EMULATOR") ;size of the window
    (setf (sdl:frame-rate) 60) ; framerate
    (sdl:with-events ()
      (:quit-event () t)
      (:idle ())
      )))

;(chip8)
