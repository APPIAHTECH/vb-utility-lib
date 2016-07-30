
Imports System
Imports System.IO

Public Class Utility

    Function esPos(numero As Integer) As Boolean
        Return numero > 0
    End Function

    Function esParrel(par As Integer)
        Return par Mod 2 = 0
    End Function

    Function esPosONeg(numero As Integer) As Boolean

        If numero > 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Sub valorAbsolut(numero As Integer)

        Dim absolut As Integer = Nothing

        If numero < 0 Then
            absolut = numero * -1
            Console.WriteLine("El valor absolut de " & numero & " es " & absolut)
        Else
            absolut = numero * 1
            Console.WriteLine("El valor absolut de " & numero & " es " & absolut)

        End If


    End Sub

    Sub minimNum(numero As Integer, numero2 As Integer)

        If numero < numero2 Then
            Console.WriteLine("El numero " & numero & "  es mes petit que el numero " & numero2)
        Else
            Console.WriteLine("El numero2 " & numero2 & " es mes petit que el numer1 " & numero)
        End If
    End Sub

    Sub maxNum(numero As Integer, numero2 As Integer)

        If numero > numero2 Then
            Console.WriteLine("El numero " & numero & "  es mes gran que el numero " & numero2)
        Else
            Console.WriteLine("El numero2 " & numero2 & " es mes gran que el numer1 " & numero)
        End If
    End Sub

    Function esEnter(numero As Decimal) As Boolean
        Return numero Mod 1 = 0
    End Function

    Function anyTraspas(any As Integer) As Boolean

        If any Mod 400 = 0 Then
            Return True
        ElseIf any Mod 4 = 0 And any Mod 100 <> 0 Then
            Return True
        Else
            Return False
        End If
    End Function

    Function quantVocals(cad As String) As String

        Dim i As Integer = Nothing
        Dim car As String = Nothing
        Dim count As Integer = 0

        cad = cad.ToLower

        For i = 1 To cad.Length

            car = GetChar(cad, i)

            If car.Equals("a") Or car.Equals("e") Or car.Equals("i") Or car.Equals("o") Or car.Equals("u") Then
                count += 1
            End If

        Next

        Return count
    End Function

    Function conteNumero(cad As String) As Boolean
        Dim i As Integer = Nothing
        Dim j As Integer = Nothing
        Dim num As String = "0123456789" '
        Dim carNum As String

        Dim car As String
        Dim teNum As Boolean = False


        For i = 1 To cad.Length

            For j = 1 To num.Length

                car = GetChar(cad, i)
                carNum = GetChar(num, j)

                If car.Equals(carNum) Then
                    teNum = True
                    Exit For
                End If
            Next
        Next

        If teNum Then
            Return True
        Else
            Return False
        End If

    End Function

    Function invertirCad(cad As String) As String

        Dim i As Integer = Nothing
        Dim rever As String = ""

        i = cad.Length

        Do Until i = 0
            rever &= GetChar(cad, i)
            i -= 1
        Loop


        Return rever

    End Function

    Function palidrom(cad As String) As String

        Dim i As Integer = Nothing
        Dim pali As String = ""

        i = cad.Length
        cad = cad.ToLower

        Do Until i = 0
            pali &= GetChar(cad, i)
            i -= 1
        Loop


        If pali.Equals(cad) Then
            Return "la cadena es un palidrom "
        Else
            Return "la cadena no es palidrom"
        End If

    End Function

    Function lletraDni(dni As Integer) As String

        Dim lletras As String = "TRWAGMYFPDXBNJZSQVHLCKE"
        Dim pos As Integer
        Dim lletra As Char

        pos = 0
        pos = dni Mod 23
        lletra = GetChar(lletras, pos + 1)

        Return "Lletra coresponent al dni : " & dni & " es : " & lletra
    End Function

    Function pasGen(Optional ByVal onlyNums As Boolean = True, Optional ByVal caracters As Boolean = True, Optional ByVal bits As Integer = 8) As String


        Dim rn As New Random()
        Dim res As String = ""

        If onlyNums <> False And caracters <> False Then

            res += rn.Next(bits)
            res += GenerateRandomString(bits, False)
            Return res

        ElseIf onlyNums = True And caracters = False Then

            res += rn.Next(bits)
            Return res

        ElseIf onlyNums = False And caracters = True Then

            res += GenerateRandomString(bits, False)
            Return res
        Else
            Return res
        End If


        Return ""

    End Function

    Public Function GenerateRandomString(ByRef len As Integer, ByRef upper As Boolean) As String

        Dim rand As New Random()

        Dim allowableChars() As Char = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLOMNOPQRSTUVWXYZ0123456789".ToCharArray()
        Dim final As String = String.Empty

        For i As Integer = 0 To len - 1
            final += allowableChars(rand.Next(allowableChars.Length - 1))
        Next

        Return IIf(upper, final.ToUpper(), final)

    End Function

    Private Function posCorreu(correu As String) As Boolean
        Return correu.Contains("@") And correu.Contains(".")
    End Function

    Function numsArray(maxNum As Integer) As String

        Dim array(maxNum) As Integer
        Dim i As Integer
        Dim nums As String = ""


        For i = 0 To array.GetUpperBound(0)
            array(i) = i
        Next

        For i = 0 To array.GetUpperBound(0)
            nums &= array(i)
        Next

        Return nums

    End Function

    Function sumaArray(array() As Integer) As Integer

        Dim i As Integer
        Dim suma As Integer = 0

        For i = 0 To array.GetUpperBound(0)
            suma += array(i)
        Next

        Return suma
    End Function

    Function validCorreu(mail() As String) As String

        Dim i As Integer
        Dim j As Integer

        Dim posAt As Integer
        Dim posPoint As Integer

        Dim tmp As String
        Dim beforAt As String
        Dim afterAt As String
        Dim domainStr As String
        Dim validMail As String = "Correus valid" & vbNewLine & "-------------------" & vbNewLine & vbNewLine
        posAt = 0
        posPoint = 0

        For i = 0 To mail.GetUpperBound(0)

            If posCorreu(mail(i)) Then
                tmp = mail(i)

                For j = 1 To tmp.Length ''obtenint pos de @

                    If GetChar(tmp, j) = "@" Then
                        posAt = j
                        Exit For
                    End If
                Next


                For j = posAt To tmp.Length ''obtenint pos de .

                    If GetChar(tmp, j) = "." Then
                        posPoint = j
                        Exit For
                    End If

                Next

                domainStr = tmp.Substring(posPoint) ''comprovacio de la longitud del doimini

                If domainStr.Length = 3 Or domainStr.Length = 2 Then

                    beforAt = tmp.Substring(0, posAt - 1)
                    afterAt = tmp.Substring(posAt)
                    afterAt = afterAt.Substring(0, afterAt.Length - domainStr.Length)
                    afterAt = afterAt.Substring(0, afterAt.Length - 1)

                    If beforAt.Contains(".") Or beforAt.Contains(" ") Or afterAt.Contains(" ") Or afterAt.Length <= 0 Then
                        ''no valid
                    Else

                        If GetChar(tmp, posAt + 1) <> "@" And GetChar(tmp, posAt - 1) <> "." Then
                            validMail += tmp & vbNewLine
                        End If

                    End If


                End If


            End If

        Next

        Return validMail

    End Function

    Sub arxiuCrear(ruta As String)

        Dim fs As FileStream

        Try
            fs = File.Create(ruta)
            Console.WriteLine("Succes")
            fs.Close()
        Catch ex As Exception
            Debug.WriteLine("error")
        End Try

    End Sub

    Public Function llegirFitxer(rutaFitxer As String)

        Try

            Dim fr As StreamReader
            fr = My.Computer.FileSystem.OpenTextFileReader(rutaFitxer)

            Dim cad As String

            cad = fr.ReadLine()

            Return cad

        Catch ex As Exception
            Console.WriteLine(ex)
        End Try

        Return ""

    End Function

    Sub arxiuEscriu(ruta As String, text As String)

        My.Computer.FileSystem.WriteAllText(ruta, text, True)

    End Sub

    Function SysMemoria() As String
        Return My.Computer.Info.TotalPhysicalMemory
    End Function

    Sub descargarFitxer(Origen As String, Desti As String)

        Try
            My.Computer.Network.DownloadFile(Origen, Desti)
        Catch ex As Exception
            Console.WriteLine(ex)
        End Try
    End Sub

    Function sensura(filtrar() As String, text As String, subs As String) As String

        Dim i As Integer = Nothing
        Dim j As Integer = Nothing
        Dim cad As String = Nothing

        Dim tmp() As String = Nothing

        tmp = text.Split(" ")

        For i = 0 To tmp.GetUpperBound(0)

            For j = 0 To filtrar.GetUpperBound(0)

                If tmp(i).Contains(filtrar(j)) Then
                    tmp(i) = subs
                End If

            Next
        Next

        For i = 0 To tmp.GetUpperBound(0)
            cad += tmp(i) & " "
        Next

        Return cad
    End Function

End Class
