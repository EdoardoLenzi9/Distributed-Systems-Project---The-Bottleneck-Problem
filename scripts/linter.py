f = open("../car/car.erl","r")
contents = f.read()
f.close() 

print(contents.replace("{", "{ ").replace("}", " }").replace("(", "( ").replace(")", " )").replace("[", "[ ").replace("]", " ]")
              .replace("{  ", "{ ").replace("  }", " }").replace("[  ", "[ ").replace("  ]", " ]").replace("(  ", "( ").replace("  )", " )")) 