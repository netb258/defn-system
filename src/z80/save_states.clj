(ns z80.save-states
  (:require [z80.memory :as memory]
            [clojure.java.io :as io])
  (:import [com.esotericsoftware.kryo Kryo]
           [com.esotericsoftware.kryo.io Output Input]
           [org.objenesis.strategy StdInstantiatorStrategy]
           [com.esotericsoftware.kryo.serializers FieldSerializer]
           [java.io FileOutputStream FileInputStream]))

(defn- configure-kryo-for-cpu
  "Returns a configured Kryo instance ready to serialize the Z80Core CPU."
  []
  (let [kryo (Kryo.)
        strategy (.getInstantiatorStrategy kryo)]
    (.setRegistrationRequired kryo false)
    ;; This StdInstantiatorStrategy solves any (no zero arg constructor) errors that might occur.
    (.setFallbackInstantiatorStrategy strategy (StdInstantiatorStrategy.))

    ;; Configure a custom field serializer for Z80Core
    (let [z80-serializer (FieldSerializer. kryo com.codingrodent.microprocessor.Z80.Z80Core)]
      ;; Explicitly remove the memory-bus and io-bus components from serialization.
      ;; They are not necessary. We only need to save CPUs internal data (registers and stuff).
      (.removeField z80-serializer "io")
      (.removeField z80-serializer "ram")

      ;; Register these strict omission rules to Kryo
      (.register kryo com.codingrodent.microprocessor.Z80.Z80Core z80-serializer))
    kryo))

(defn- get-vdp-class []
  (Class/forName "z80.vdp.VdpState"))

(defn- configure-kryo-for-vdp
  "Returns a configured Kryo instance ready to serialize the VDP (which is a Clojure record)."
  []
  (let [kryo (Kryo.)
        vdp-cls (get-vdp-class)
        strategy (.getInstantiatorStrategy kryo)]
    ;; Allow Kryo to find and use the record class implicitly
    (.setRegistrationRequired kryo false)
    ;; This StdInstantiatorStrategy solves any (no zero arg constructor) errors that might occur.
    (.setFallbackInstantiatorStrategy strategy (StdInstantiatorStrategy.))

    ;; Explicitly register the Clojure Record class using a standard FieldSerializer.
    ;; This ensures Kryo treats the record like a regular Java object with fields.
    (.register kryo vdp-cls (FieldSerializer. kryo vdp-cls))
    kryo))

(defn- save-cpu-state
  "Uses Kryo to serialize a Z80Core CPU (state-data) to a file (file-path)."
  [file-path state-data]
  (let [kryo (configure-kryo-for-cpu)]
    (with-open [fos (FileOutputStream. file-path)
                output (Output. fos)]
      (.writeObject kryo output state-data))))

(defn- load-cpu-state
  "Uses Kryo to deserialize a Z80Core object from a file (file-path).
   Note that the current memory-bus and io-bus must be passed as arguments.
   As noted in (configure-kryo-for-cpu) these components do NOT get serialized along with the CPU.
   This means we have to plug them into the CPU when we deserialize."
  [file-path memory-bus io-bus]
  (let [kryo (configure-kryo-for-cpu)
        class-type com.codingrodent.microprocessor.Z80.Z80Core]
    (with-open [fis (FileInputStream. file-path)
                input (Input. fis)]
      (let [loaded-cpu (.readObject kryo input class-type)
            io-field   (.getDeclaredField class-type "io")
            ram-field  (.getDeclaredField class-type "ram")]
        ;; 1. Manually re-inject the io-bus using reflection
        (.setAccessible io-field true)
        (.set io-field loaded-cpu io-bus)
        ;; 2. Manually re-inject the memory-bus using reflection
        (.setAccessible ram-field true)
        (.set ram-field loaded-cpu memory-bus)
        loaded-cpu))))

(defn- save-vdp-state
  "Uses Kryo to serialize a VDP record (state-data) to a file (file-path)."
  [file-path vdp-record]
  (let [kryo (configure-kryo-for-vdp)]
    (with-open [fos (FileOutputStream. file-path)
                output (Output. fos)]
      (.writeObject kryo output vdp-record))))

(defn- load-vdp-state
  "Uses Kryo to deserialize a VDP record from a file (file-path)."
  [file-path]
  (let [kryo (configure-kryo-for-vdp)]
    (with-open [fis (FileInputStream. file-path)
                input (Input. fis)]
      ;; Explicitly casting to the Record type upon reading
      (.readObject kryo input (get-vdp-class)))))

(defn- create-sstates-directory!
  "Creates the save-states directory, but only if it does not already exist.
   Returns true if created and false if it already exists"
  []
  (.mkdirs (io/file "./save-states/")))

(defn save-state
  "Serializes all key components to disk (CPU, VDP, RAM, MAPPER-BANKS, SRAM).
   These are all the components necessary to capture the emulator's current state.
   Each component is saved in it's own file inside the ./save-states/ directory."
  [cpu-atom vdp-atom]
  (create-sstates-directory!)
  (let [base-file-path        (str "./save-states/" (memory/get-rom-md5-hash))]
    (save-cpu-state           (str base-file-path ".cpu") @cpu-atom)
    (save-vdp-state           (str base-file-path ".vdp") @vdp-atom)
    (memory/serialize-ram!    (str base-file-path ".memory"))
    (memory/serialize-sram!   (str base-file-path ".sram"))
    (memory/serialize-mapper! (str base-file-path ".mbanks"))))

(defn load-state
  "Deserializes all key components from disk (CPU, VDP, RAM, MAPPER-BANKS, SRAM).
   This essentially resumes a game from the state saved on disk."
  [cpu-atom vdp-atom memory-bus-atom io-bus-atom]
  (let [base-file-path (str "./save-states/" (memory/get-rom-md5-hash))]
    (reset! cpu-atom (load-cpu-state (str base-file-path ".cpu") @memory-bus-atom @io-bus-atom))
    (reset! vdp-atom (load-vdp-state (str base-file-path ".vdp")))
    (memory/deserialize-ram!    (str base-file-path ".memory"))
    (memory/deserialize-sram!   (str base-file-path ".sram"))
    (memory/deserialize-mapper! (str base-file-path ".mbanks"))))
