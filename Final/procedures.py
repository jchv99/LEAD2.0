class Function:
    name: str
    typeFunc: str
    memory: int
    paramNum: int
    inicia: int
    termina: int
    contInt: int
    contFloat: int
    contChar: int
    contBool: int
    endf: int

    varTable = {}

    def __init__(self, nameTemp, typeTemp):
        self.name = nameTemp
        self.typeFunc = typeTemp
        self.varTable = {}
        self.paramNum = 0
        self.memory = 0
        self.inicia = 0
        self.contInt = 0
        self.contFloat = 0
        self.contBool = 0
        self.contChar = 0
        self.termina = 0

class Variable:
    name: str
    typeVar: str
    add: int
    val: str

    def __init__(self, nameTemp, typeTemp, addTemp):
        self.name = nameTemp
        self.typeVar = typeTemp
        self.add = addTemp
        self.val = ""

class FunctionDirectory:
    def __init__(self):
        self.functionTable = {"global": Function("global", "void")}
        self.variableTable = { }
        self.variableTable["global"] = { } 
        # No se utiliza
        self.typeTempFunc = ""
    
    def addFunc(self, nameTemp, typeTemp):
        if nameTemp == "main":
            # Se inserta la funcion en la tabla de funciones
            self.functionTable[nameTemp] = Function(nameTemp, "void")
            # Se crea la tabla para las variables de esa funcion
            self.variableTable[nameTemp] = { }
        else:
            # Se inserta la funcion en la tabla de funciones
            self.functionTable[nameTemp] = Function(nameTemp, typeTemp)
            # Se crea la tabla para las variables de esa funcion            
            self.variableTable[nameTemp] = { }

    def addVar(self, currScope, nameTemp, typeTemp, addTemp):
        self.variableTable[self.functionTable[currScope].name][nameTemp] = Variable(nameTemp, typeTemp, addTemp)
        pass

    def addMemory(self, currScope):
        self.functionTable[currScope].memory = len(self.variableTable[currScope])
        pass

    def printMem(self, currScope):
        return self.functionTable[currScope].memory
    
    def printParam(self, currScope):
        return self.functionTable[currScope].paramNum
    

