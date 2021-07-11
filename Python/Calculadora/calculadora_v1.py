# Calculadora em Python

# Desenvolva uma calculadora em Python com tudo que você aprendeu nos capítulos 2 e 3. 
# A solução será apresentada no próximo capítulo!
# Assista o vídeo com a execução do programa!

print("\n******************* Python Calculator *******************\n")


def soma(x,y):
    return x+y; 

def subtracao(x,y):
    return x-y; 

def multiplicacao(x,y):
    return x*y; 

def divisao(x,y):
    return x/y; 

opcao = input("Selecione o número da operação desejada: \n1 - Soma\n2 - Subtração\n3 - Multiplicação\n4 - Divisão\n\nDigite sua opção(1/2/3/4): ") 

primeiroNum = int(input("\nDigite o primeiro número: "))
segundoNum = int(input("\nDigite o segundo número: "))

if(opcao == "1"):
    print("\n", primeiroNum, "+", segundoNum, "=", soma(primeiroNum, segundoNum))
elif(opcao == "2"):
    print("\n", primeiroNum, "-", segundoNum, "=", subtracao(primeiroNum, segundoNum))
elif(opcao == "3"):
    print("\n", primeiroNum, "*", segundoNum, "=", multiplicacao(primeiroNum, segundoNum))
elif(opcao == "4"):
    print("\n", primeiroNum, "/", segundoNum, "=", divisao(primeiroNum, segundoNum))
else: 
    print("Opção Inválida!")
