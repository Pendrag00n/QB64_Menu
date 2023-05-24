Screen 12
'Declarancion de variables:
View Screen(30, 50)-(610, 425), 0, 1
Color 3: Locate 7, 32: Print "Menu de Programas"
Color 2: Locate 12, 12: Print "1 - Calculo de Areas"
Color 2: Locate 16, 12: Print "2 - Ecuacion 2 grado"
Color 2: Locate 20, 12: Print "3 - Sistema 3x3"
Color 2: Locate 12, 50: Print "4 - Estadistica"
Color 2: Locate 16, 50: Print "5 - Circulo"
Color 6: Locate 20, 45: Print "Escoja un Programa"

Do
    Color 3: Locate 25, 50: Print Time$
    c$ = InKey$
Loop Until c$ = "x" Or c$ = "1" Or c$ = "2" Or c$ = "3" Or c$ = "4" Or c$ = "5"

Select Case c$
    Case "1"
        Cls
        Call areas
    Case "2"
        Cls
        Call ec2
    Case "3"
        Cls
        Call sistema3x3
    Case "4"
        Cls
        Call est
    Case "5"
        Cls
        Call circl
End Select

'S U B P R O G R A M A S ////////////////////////////////////////////////////////////////////////////////////////////////

Sub areas
    'Script que calcula el area y el perimetro de ciertas figuras geometricas
    Dim X As Integer
    Dim r, BT, HT, LT1, LT2, BC, AC, LP, AP As _Unsigned Integer
    PI = 3.1415926535897

    Do
        Cls
        Locate 6, 12: Print "1 CIRCULO        2 TRIANGULO"
        Locate 8, 12: Print "3 RECTANGULO     4 PENTAGONO"
        Locate 12, 12: Print "(Introduzca las medidas en centimetros)"
        Locate 16, 14: Input "SELECCIONE LA FIGURA = ", X
    Loop Until X = 1 Or X = 2 Or X = 3 Or X = 4
    Select Case X
        Case 1

            Cls
            Line (350, 20)-(350, 600), 1
            Locate 6, 8: Input "Radio de la circunferencia = ", r
            Call carga
            ACI = PI * r * r
            PCI = 2 * PI * r
            Locate 13, 49: Print "Area = "; ACI; "cm2"
            Locate 16, 49: Print "Perimeto = "; PCI; "cm"

        Case 2
            Do
                Cls
                Line (350, 20)-(350, 600), 1
                Locate 6, 8: Input "Base = ", BT
                Locate 8, 8: Input "Altura = ", HT
                Locate 10, 8: Input "Primer Lado = ", LT1
                Locate 12, 8: Input "Segundo Lado = ", LT2
            Loop Until BT > 0 And HT > 0 And LT1 > 0 And LT2 > 0
            AT = (BT * HT) / 2
            PT = BT + LT1 + LT2
            Call carga
            Locate 13, 49: Print "Area = "; AT; "cm2"
            Locate 16, 49: Print "Perimetro = "; PT; "cm"

        Case 3
            Do
                Cls
                Line (350, 20)-(350, 600), 1
                Locate 6, 8: Input "Primer Lado = ", BC
                Locate 8, 8: Input "Segundo Lado = ", AC
            Loop Until BC > 0 And AC > 0
            ArC = BC * AC
            PeC = 2 * BC + 2 * AC
            Call carga
            Locate 13, 49: Print "Area = "; ArC; "cm2"
            Locate 16, 49: Print "Perimetro = "; PeC; "cm"

        Case 4
            Do
                Cls
                Line (350, 20)-(350, 600), 1
                Locate 6, 8: Input "Lado = ", LP
                Locate 8, 8: Input "Apotema = ", AP
            Loop Until LP > 0 And AP > 0
            PP = LP * 5
            ArP = (PP * AP) / 2
            Call carga
            Locate 13, 49: Print "Area = "; ArP; "cm2"
            Locate 16, 49: Print "Perimetro = "; PP; "cm"

    End Select
    End
End Sub

'///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Sub ec2
    Line (350, 20)-(350, 600), 1
    Locate 6, 8: Print "Introduce los valores de A, B y C"
    Locate 9, 8: Input "Introduce el valor de A = ", A
    Locate 11, 8: Input "Introduce el valor de B = ", B
    Locate 13, 8: Input "Introduce el valor de C = ", C
    D = B * B - 4 * A * C
    X1 = (-B + Sqr(D)) / 2 * A
    X2 = (-B - Sqr(D)) / 2 * A
    If D < 0 Then
        Call carga
        Locate 13, 50: Print "N/A solucion real!"
        Locate 17, 50: Print "X1 equivale a "; X1 + X2; "I"
        Locate 20, 50: Print "X2 equivale a "; X1 - X2; "I"
    Else
        Call carga
        Locate 17, 50: Print "X1 equivale a "; X1
        Locate 20, 50: Print "X2 equivale a "; X2
    End If
    End
End Sub

'///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Sub sistema3x3
    Line (350, 20)-(350, 600), 1
    Locate 12, 8: Print "La ecuacion debe tener este formato:"
    Locate 14, 8: Print "a11 X + a12 Y + a13 Z = A1"
    Locate 15, 8: Print "a21 X + a22 Y + a23 Z = A2"
    Locate 16, 8: Print "a31 X + a32 Y + a33 Z = A3"
    Locate 20, 8: Input "Introudzca el valor de a11: ", a11
    Locate 20, 8: Input "Introudzca el valor de a12: ", a12
    Locate 20, 8: Input "Introudzca el valor de a13: ", a13
    Locate 20, 8: Input "Valor de la primera ecuacion: ", a1
    Cls
    Line (350, 20)-(350, 600), 1
    Locate 12, 8: Print "La ecuacion debe tener este formato:"
    Locate 14, 8: Print "a11 X + a12 Y + a13 Z = A1"
    Locate 15, 8: Print "a21 X + a22 Y + a23 Z = A2"
    Locate 16, 8: Print "a31 X + a32 Y + a33 Z = A3"
    Locate 20, 8: Input "Introudzca el valor de a21: ", a21
    Locate 20, 8: Input "Introudzca el valor de a22: ", a22
    Locate 20, 8: Input "Introudzca el valor de a23: ", a23
    Locate 20, 8: Input "Valor de la segunda ecuacion: ", A2
    Cls
    Line (350, 20)-(350, 600), 1
    Locate 12, 8: Print "La ecuacion debe tener este formato:"
    Locate 14, 8: Print "a11 X + a12 Y + a13 Z = A1"
    Locate 15, 8: Print "a21 X + a22 Y + a23 Z = A2"
    Locate 16, 8: Print "a31 X + a32 Y + a33 Z = A3"
    Locate 20, 8: Input "Introudzca el valor de a31: ", a31
    Locate 20, 8: Input "Introudzca el valor de a32: ", a32
    Locate 20, 8: Input "Introudzca el valor de a33: ", a33
    Locate 20, 8: Input "Valor de la tercera ecuacion: ", A3
    Cls
    Line (350, 20)-(350, 600), 1

    D = (a11 * a22 * a33) + (a12 * a23 * a31) + (a32 * a21 * a13) - (a13 * a22 * a31) - (a11 * a23 * a32) - (a12 * a21 * a33)
    Line (350, 20)-(350, 600), 1
    If D = 0 Then
        Call carga
        Locate 13, 49: Print "El Determinante es 0"
        Locate 14, 49: Print "No tiene Solucion"
    Else
        Call carga
        Locate 13, 47: Print "El Determinante es "; D
        X = ((a1 * a22 * a33) + (a13 * A2 * a32) + (a12 * a23 * A3) - (a1 * a32 * a23) - (a12 * A2 * a33) - (a13 * a22 * A3)) / D
        Y = ((a11 * A2 * a33) + (a1 * a23 * a31) + (a13 * a21 * A3) - (a13 * A2 * a31) - (a1 * a21 * a33) - (a11 * a23 * A3)) / D
        4
        Z = ((a11 * a22 * A3) + (a12 * A2 * a31) + (a1 * a21 * a32) - (a1 * a22 * a31) - (a11 * A2 * a32) - (a12 * a21 * A3)) / D
        Locate 15, 49: Print "X Equivale a "; X
        Locate 16, 49: Print "Y Equivale a "; Y
        Locate 17, 49: Print "Z Equivale a "; Z
    End If
    End
End Sub

'///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
Sub est
    Dim t As Double
    Dim I, x(1000), f(1000), Moda, Modaa, Mediana, Desv1, Sumo As Single
    Line (350, 20)-(350, 600), 1
    Locate 8, 7: Input "Introduzca numero total de datos: ", N
    Cls
    Line (350, 20)-(350, 600), 1
    Locate 8, 7: Print "Introduzca datos en orden creciente"

    For I = 1 To N
        Locate 10, 7: Input "Dato= "; x(I)
        Locate 11, 7: Input "Veces que se repite = "; f(I)
    Next I

    Moda = 0

    For I = 0 To N
        If Modaa < f(I) Then
            Modaa = f(I)
            Moda = x(I)
        Else
            Moda = Moda
        End If

    Next I

    For j = 0 To N
        t = t + x(j) * f(j)

    Next j

    For l = 0 To N
        ff = ff + f(l)

    Next l
    media = t / ff

    For I = 0 To N
        Sumo = Sumo + (((x(I) - media) ^ 2) * f(I))
    Next I
    Desv1 = Sumo / (ff - 1)
    desv2 = Desv1 ^ 0.5


    medianaa = ff / 2
    medianaaa = ff \ 2

    If medianaa = medianaa Then

        Mediana = x(medianaa - 1) + 0.5
    Else
        Mediana = x(medianaa - 1)

    End If
    Line (350, 20)-(350, 600), 1
    Call carga
    Locate 15, 48: Print "Total ="; t
    Locate 16, 48: Print "Moda ="; Moda
    Locate 17, 48: Print "Media ="; media
    Locate 18, 48: Print "Mediana ="; Mediana
    Locate 19, 48: Print "Desviacion ="; desv2
    End
End Sub

'////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

Sub circl
    Dim PULSADO As String
    Dim Y As Integer
    Dim X As Integer

    'Declaracion de variables:
    X = 300
    Y = 210
    V = 5
    a$ = Date$
    day$ = Mid$(a$, 4, 2)
    month$ = Mid$(a$, 1, 2)
    year$ = Mid$(a$, 9, 2)

    'Portada del programa: (Los prints seguidos se podrian haber puesto en una sola linea
    Cls
    Print
    Print
    Print
    Print
    Print
    Print
    Print
    Color 2: Print "                 Puedes mover el circulo con las teclas W A S D"
    Print
    Color 2: Print "                Puedes cambiar la velocidad con las teclas + y -"
    Print
    Print
    Print
    Color 2: Print "            1 = AZUL   2 = VERDE   3 = CYAN   4 = ROJO   5 = VIOLETA"
    Print
    Color 2: Input "                      De que color quieres tu circulo"; c
    Cls
    Screen 0
    Screen 12
    'Codigo para el correcto funcionamiento del los elemento decorativos y el movimiento del circulo:
    Cls

    Do

        Locate 25, 13: Print "Velocidad = "; V; "Px/s"
        Locate 25, 60: Print Time$
        Locate 25, 50: Print day$; "/"; month$; "/"; year$
        'El movimiento del circulo no rompe ningun print
        Line (600, 425)-(40, 60), 15, B
        PULSADO = UCase$(InKey$)
        If PULSADO = "W" And Y > 115 Then
            Cls
            Y = Y - V
        ElseIf PULSADO = "S" And Y < 425 - 55 Then
            Cls
            Y = Y + V
        ElseIf PULSADO = "A" And X > 95 Then
            Cls
            X = X - V
        ElseIf PULSADO = "D" And X < 600 - 55 Then
            Cls
            X = X + V
        End If
        'El programa no permite el movimiento mas alla de la direccion de cada borde al estar todo definido en multiplos de 5
        If PULSADO = "+" Then
            V = V + 5
        ElseIf PULSADO = "-" Then
            V = V - 5
        End If
        'Si la velocidad no va de 5 en 5 el circulo puede saltarse los bordes

        Circle (X, Y), 50, c
        Circle (X, Y), 51, c
        Circle (X, Y), 52, c
        Circle (X, Y), 53, c
        Circle (X, Y), 54, c
        Circle (X, Y), 55, c
        'Varios circulos sucesivos crean la ilusion de un circulo mas grueso que da un mejor aspecto al programa
    Loop Until InKey$ = "x"
    End
    'El comando para salir del programa no siempre responde correctamente, aun no se a que se debe, sucede de forma pseudoaleatoria
End Sub

Sub carga
    linea = 400
    Line (linea, 380)-(560, 400), 3, B
    Do
        linea = linea + 1
        Line (linea, 380)-(560, 400), 3, B
        _Delay 0.005
    Loop Until linea >= 560
    Cls
    Line (350, 20)-(350, 600), 1
End Sub
