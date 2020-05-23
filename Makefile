RUN := docker run -v $(PWD):/src -w /src
IMAGE := haskell:8
EXECUTABLE := enigma

all: test build clean encode

test: Main.hs Machine.hs Spec.hs
	$(RUN) -v $(PWD)/.cabal:/root/.cabal:delegated -v $(PWD)/.ghc:/root/.ghc:delegated $(IMAGE) runhaskell Spec.hs

build: Main.hs Machine.hs
	$(RUN) $(IMAGE) ghc -Wall -o $(EXECUTABLE) Main.hs

clean:
	rm *.hi *.o

init:
	$(RUN) -v $(PWD)/.cabal:/root/.cabal:delegated -v $(PWD)/.ghc:/root/.ghc:delegated $(IMAGE) bash -c "cabal update && cabal install hspec QuickCheck --lib"

rotors?=321
reflector?=b
message=helloworld
encode:
ifdef plugboard
	$(RUN) $(IMAGE) ./$(EXECUTABLE) -rotors $(rotors) -reflector $(reflector) -plugboard $(plugboard) -message $(message)
else
	$(RUN) $(IMAGE) ./$(EXECUTABLE) -rotors $(rotors) -reflector $(reflector) -message $(message)
endif

shell:
	$(RUN) -it $(IMAGE) bash
