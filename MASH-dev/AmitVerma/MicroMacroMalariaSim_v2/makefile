# Makefile for MicroMacroMalariaSim_v1.* 

CC = g++
CFLAGS = -g -std=c++11
SRCMAC = src/macrosim
SRCMIC = src/microsim
OBJDIRMIC = obj/microsim
OBJDIRMAC = obj/macrosim
#INCDIR = include
#VPATH = $(SRCDIR) $(INCDIR)

_OBJSMIC = MalariaSim.o Mosquito.o Patch.o Pond.o House.o HumanV2.o location.o Distance.o HealthClinic.o Drug.o PF.o RXItem.o
OBJSMIC = $(patsubst %,$(OBJDIRMIC)/%,$(_OBJSMIC))
_OBJSMAC = main.o MacroSimulator.o DistHazardsModel.o SimpleRossMacdonaldModel.o Patch.o Human.o
OBJSMAC = $(patsubst %,$(OBJDIRMAC)/%,$(_OBJSMAC))

all: MacroSim MicroSim
	
MicroSim: $(OBJSMIC)
	$(CC) $(OBJSMIC) -o MicroSim
	
MacroSim: $(OBJSMAC)
	$(CC) $(OBJSMAC) -o MacroSim
	
$(OBJDIRMIC)/%.o: $(SRCMIC)/%.cpp
	$(CC) -c $(CFLAGS) -o $@ $<

$(OBJDIRMAC)/%.o: $(SRCMAC)/%.cpp
	$(CC) -c $(CFLAGS) -o $@ $<	

clean : 
	rm $(OBJSMIC) $(OBJSMAC)
