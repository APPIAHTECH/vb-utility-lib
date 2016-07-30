Imports System.IO
Imports System.Windows.Forms
Public Class Primaria

    Private ruta As String

    Public Sub New()

        Dim file As New OpenFileDialog()

        Console.WriteLine("Introduiex exercisi per resoldre , ruta :)")
        file.ShowDialog()
        Me.ruta = file.FileName()

        Select Case accioEx()

            Case "suma"
                Console.WriteLine("Procediment : " & accioEx() & " : IDEX : " & idEx())
                suma()

            Case "resta"
                Console.WriteLine("Procediment : " & accioEx() & " : IDEX : " & idEx())
                resta()

            Case "multi"
                Console.WriteLine("Procediment : " & accioEx() & " : IDEX : " & idEx())
                multi()

            Case "div"
                Console.WriteLine("Procediment : " & accioEx() & " : IDEX : " & idEx())
                div()
            Case Else
                Console.WriteLine("opcio no trobada o mal escrit o espais no adequats (acorta els anunciat ex : divicio -> div)" & accioEx())
        End Select

    End Sub

    Public Function getRuta()
        Return Me.ruta
    End Function

    Public Sub setRuta(rutaNew As String)
        Me.ruta = rutaNew
    End Sub

    Private Function idEx() As String

        Dim doc As String = Nothing
        Dim id As String = Nothing
        Dim val() As String

        Try
            doc = llegirFitxer(Me.ruta)
            val = doc.Split(" ")

            id = val(0)
            id = id.Trim()

            Return id

        Catch ex As Exception
            Console.WriteLine(ex)
        End Try

        Return id
    End Function

    Private Function accioEx()

        Dim doc As String = Nothing
        Dim valors() As String = Nothing
        Dim accio As String = Nothing

        Try
            doc = llegirFitxer(Me.ruta)
            valors = doc.Split(" ")
            accio = valors(1)
            accio = accio.Trim()
            accio = accio.ToLower()

            Return accio

        Catch ex As Exception
            Console.WriteLine(ex)
        End Try

        Return accio

    End Function

    Private Sub suma()

        Dim doc As String = Nothing
        Dim valors() As String

        Dim i As Integer
        Dim val As Decimal

        val = 0

        Try
            doc = llegirFitxer(Me.ruta)
            valors = doc.Split()

            For i = 2 To valors.GetUpperBound(0)


                If esNum(valors(i)) Then
                    val += CStr(valors(i))
                Else
                    If valors(i) = "=" Then
                        Console.WriteLine(val)
                        ''arxiu amb els valor correctes
                        val = 0
                    End If
                End If

            Next

        Catch ex As Exception
            Console.WriteLine(ex)
        End Try

    End Sub

    Private Sub resta()

        Dim doc As String = Nothing
        Dim valors() As String

        Dim i As Integer
        Dim res As Decimal
        Dim res2 As Decimal = Nothing
        Dim tmp As String = Nothing

        doc = llegirFitxer(Me.ruta)
        valors = doc.Split()


        For i = 2 To valors.GetUpperBound(0)

            If esNum(valors(i)) Then

                If valors(i) = tmp And valors(i + 1) = "=" Then
                    Console.WriteLine(res2) 'escriu res 
                    res = 0
                    res2 = Nothing
                    Continue For
                Else
                    tmp = esValidNum(valors, i + 1)

                    If res2 = Nothing Then
                        res = CStr(valors(i)) - CStr(esValidNum(valors, i + 1))
                        res2 = res
                    Else
                        res2 = res2 - tmp
                    End If

                End If
            End If

        Next

    End Sub

    Private Sub div()

        Dim doc As String = Nothing
        Dim valors() As String

        Dim i As Integer
        Dim div As Decimal
        Dim div2 As Decimal = Nothing
        Dim tmp As String = Nothing

        doc = llegirFitxer(Me.ruta)
        valors = doc.Split()


        For i = 2 To valors.GetUpperBound(0)

            If esNum(valors(i)) Then

                If valors(i) = tmp And valors(i + 1) = "=" Then
                    Console.WriteLine(div2) 'escriu res 
                    div = 0
                    div2 = Nothing
                    Continue For
                Else
                    tmp = esValidNum(valors, i + 1)

                    If div2 = Nothing Then
                        div = CStr(valors(i)) / CStr(esValidNum(valors, i + 1))
                        div2 = div
                    Else
                        div2 = div2 / tmp
                    End If

                End If
            End If

        Next

    End Sub

    Private Sub multi()

        Dim doc As String = Nothing
        Dim valors() As String

        Dim i As Integer
        Dim mutli As Decimal
        Dim mutli2 As Decimal = Nothing
        Dim tmp As String = Nothing

        doc = llegirFitxer(Me.ruta)
        valors = doc.Split()


        For i = 2 To valors.GetUpperBound(0)

            If esNum(valors(i)) Then

                If valors(i) = tmp And valors(i + 1) = "=" Then
                    Console.WriteLine(mutli2) 'escriu res 
                    mutli = 0
                    mutli2 = Nothing
                    Continue For
                Else
                    tmp = esValidNum(valors, i + 1)

                    If mutli2 = Nothing Then
                        mutli = CStr(valors(i)) * CStr(esValidNum(valors, i + 1))
                        mutli2 = mutli
                    Else
                        mutli2 = mutli2 * tmp
                    End If

                End If
            End If

        Next

    End Sub

    Private Function llegirFitxer(ruta As String)

        Try

            Dim fr As StreamReader
            fr = My.Computer.FileSystem.OpenTextFileReader(ruta)

            Dim cad As String = Nothing

            While fr.Peek() >= 0
                cad += fr.ReadLine

            End While

            Return cad

        Catch ex As Exception
            Console.WriteLine(ex)
            Return ""
        End Try



    End Function

    Private Function esNum(val As String) As Boolean

        Dim nums As String = "0123456789"
        Dim i As Integer
        Dim j As Integer
        Dim esNu As Boolean = False

        For i = 1 To val.Length

            For j = 1 To nums.Length
                If GetChar(val, i) = GetChar(nums, j) Then
                    esNu = True
                End If
            Next
        Next

        Return esNu

    End Function

    Private Function esValidNum(arr() As String, pos As Integer) As String

        Dim i As Integer

        For i = pos To arr.GetUpperBound(0)
            If esNum(arr(i)) Then
                Return arr(i)
            End If
        Next

        Return ""
    End Function

End Class
