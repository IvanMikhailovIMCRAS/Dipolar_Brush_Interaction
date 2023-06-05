# имя программы:
TARGET = DIP

# исходники для сборки:
SOURCES = \
       CommonParam.f90  \
       ErrorList.f90 \
       Lib.f90 \
       InputOutput.f90 \
       OnePoint.f90  \
       DIP.f90

OBJECTS=$(SOURCES:%.f90=%.o)

TYPE=master

# про флаги:
# https://www.opennet.ru/docs/RUS/cpp/cpp-10.html

# простая сборка:
all: $(TARGET)

$(OBJECTS): $(SOURCES)

$(TARGET): $(OBJECTS)
	gfortran -O3 -o $(TARGET) -cpp $(SOURCES) 
clean:
	$(RM) $(TARGET) *.mod
	
.PHONY: all clean