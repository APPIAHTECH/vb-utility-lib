Public Class Persona

    Private nom As String
    Private edat As Integer
    Private pais As String


    Sub New(ByVal nom As String, ByVal edat As String, ByVal pais As String)

        Me.nom = nom
        Me.edat = edat
        Me.pais = pais

    End Sub

    Public Function getNom() As String
        Return Me.nom
    End Function

    Public Function getEdat() As String
        Return Me.edat
    End Function

    Public Function getPais() As String
        Return Me.pais
    End Function

    Public Sub setNom(nom As String)
        Me.nom = nom
    End Sub

    Public Sub setEdat(eda As Integer)
        Me.edat = eda
    End Sub

    Public Sub setPais(pais As String)
        Me.pais = pais
    End Sub


End Class
