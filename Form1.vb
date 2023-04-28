Public Class Form1
    Const _decBmi As Decimal = 10D
    Const _decGcuon As Decimal = 10D
    Const _decPho As Decimal = 15D
    Const _decBxeo As Decimal = 12D
    Const _decBcha As Decimal = 15D
    Const _decBbao As Decimal = 15D
    Const _decCoke As Decimal = 2.5D
    Const _decSprite As Decimal = 3D
    Const _decJuice As Decimal = 3.5D
    Const _decTea As Decimal = 4D
    Const _decBoba As Decimal = 4.5D
    Const _decCpsd As Decimal = 5D


    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cboTip.SelectedIndexChanged
        Dim total As Decimal
        Dim decPrice As Decimal

        If cboTip.SelectedIndex = 0 Then
            decPrice = Convert.ToDecimal(txtPrice.Text)
            total = decPrice + (decPrice * 0.05D)
        ElseIf cboTip.SelectedIndex = 1 Then
            decPrice = Convert.ToDecimal(txtPrice.Text)
            total = decPrice + (decPrice * 0.1D)
        ElseIf cboTip.SelectedIndex = 2 Then
            decPrice = Convert.ToDecimal(txtPrice.Text)
            total = decPrice + (decPrice * 0.15D)
        ElseIf cboTip.SelectedIndex = 3 Then
            decPrice = Convert.ToDecimal(txtPrice.Text)
            total = decPrice + (decPrice * 0.2D)
        ElseIf cboTip.SelectedIndex = 4 Then
            decPrice = Convert.ToDecimal(txtPrice.Text)
            total = decPrice + (decPrice * 0.25D)
        ElseIf cboTip.SelectedIndex = 5 Then
            decPrice = Convert.ToDecimal(txtPrice.Text)
            total = decPrice + (decPrice * 0.5D)
        ElseIf cboTip.SelectedIndex = 6 Then
            decPrice = Convert.ToDecimal(txtPrice.Text)
            total = decPrice + decPrice
        End If
        txtTotal.Text = total.ToString("F2")
    End Sub

    Private Function ValidateInput() As Boolean
        'validate Input
        Dim intNumber As Integer
        Dim blnValid As Boolean = False
        Try
            'convert to an integer value
            intNumber = Convert.ToInt32(txtPark.Text)
            If intNumber > 0D Then
                blnValid = True
                Return blnValid
            Else
                MsgBox("Please enter a number greater than 0", , "Error")
            End If

        Catch Exception As FormatException
            'case of symbols, letters,...
            MsgBox("Please enter a valid amount", , "Error")
        Catch Exception As SystemException
            'case of the rest
            MsgBox("Entry invalid. Please enter a number", , "Error")

        End Try
        txtPark.Focus()
        txtPark.Clear()
        Return blnValid
    End Function
    Private Sub TextBox2_TextChanged(sender As Object, e As EventArgs) Handles txtPark.TextChanged
        Dim intQuarter As Integer = 0
        Dim intTime As Integer = 15
        Dim intParkingTime As Integer
        Dim intAmount As Decimal
        Dim blnAmountIsValid As Boolean = False
        blnAmountIsValid = ValidateInput()
        If blnAmountIsValid = True Then
            intAmount = Convert.ToInt32(txtPark.Text)
            Do Until intAmount = 0
                intQuarter += 1
                intAmount -= 1
            Loop
        End If
        intParkingTime = intQuarter * intTime
        lblParking.Text = "You can park here in: " & intParkingTime.ToString() & " minutes"
        lblFee.Visible = False
    End Sub

    Private Sub cboPayment_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cboPayment.SelectedIndexChanged
        Dim cash As Decimal
        Dim change As Decimal
        Dim total As Decimal
        If IsNumeric(cash) Then
            cash = Convert.ToDecimal(cash)
            If cboPayment.SelectedIndex = 0 Then
                total = Convert.ToDecimal(txtTotal.Text)
                cash = InputBox("Enter Cash")
                change = cash - total


                MsgBox("Change is " & change.ToString("C"))
            End If
            If cboPayment.SelectedIndex = 1 Or cboPayment.SelectedIndex = 2 Then
                MsgBox("Thank you for your payment")
            End If
        Else
            MsgBox("Enter a number")
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnConfirm.Click
        Dim name As String
        Dim total As String
        name = Convert.ToString(txtName.Text)
        total = Convert.ToString(txtTotal.Text)
        MsgBox("Thank you " & name & " for choosing us, your order total is " & total)
    End Sub


    Private Sub btnReset_Click(sender As Object, e As EventArgs) Handles btnReset.Click
        ClearForm()
    End Sub
    Private Sub ClearForm()
        'Clear Form
        chbBmi.Checked = False
        chbGcuon.Checked = False
        chbPho.Checked = False
        ChbBxeo.Checked = False
        chbBcha.Checked = False
        chbBbao.Checked = False
        chbCoke.Checked = False
        chbSprite.Checked = False
        chbJuice.Checked = False
        chbTea.Checked = False
        chbBoba.Checked = False
        chbCPSD.Checked = False
        txtBbao.Visible = False
        txtBcha.Visible = False
        txtGcuon.Visible = False
        txtPho.Visible = False
        txtBxeo.Visible = False
        txtBmi.Visible = False
        txtCoke.Visible = False
        txtSprite.Visible = False
        txtJuice.Visible = False
        txtTea.Visible = False
        txtBoba.Visible = False
        txtCPSD.Visible = False
        txtName.Clear()
        txtPark.Clear()
        txtPark.Focus()
        txtPrice.Clear()
        txtTotal.Clear()
        lblParking.Text = ""
        cboTip.SelectedIndex = -1
        cboPayment.SelectedIndex = -1


    End Sub

    Private Sub btnExit_Click(sender As Object, e As EventArgs) Handles btnExit.Click
        Close()
    End Sub
    Private Sub btnCalculate_Click(sender As Object, e As EventArgs) Handles btnCalculate.Click
        Dim sum As Decimal
        Dim bmi As Integer
        Dim gcuon As Integer
        Dim pho As Integer
        Dim bxeo As Integer
        Dim bcha As Integer
        Dim bbao As Integer
        Dim coke As Integer
        Dim sprite As Integer
        Dim juice As Integer
        Dim tea As Integer
        Dim boba As Integer
        Dim cpsd As Integer

        If chbBmi.Checked = True Then
            If IsNumeric(txtBmi.Text) Then
                bmi = Convert.ToInt32(txtBmi.Text)
                sum += bmi * _decBmi
            Else
                MsgBox("Enter a number of Banh Mi")
            End If
        Else
            txtBmi.Text = "0"
        End If
        If chbGcuon.Checked = True Then
            If IsNumeric(txtGcuon.Text) Then
                gcuon = Convert.ToInt32(txtGcuon.Text)
                sum += bmi * _decGcuon
            Else
                MsgBox("Enter a number of Goi Cuon")
            End If
        Else
            txtGcuon.Text = "0"
        End If
        If chbPho.Checked = True Then
            If IsNumeric(txtPho.Text) Then
                pho = Convert.ToInt32(txtPho.Text)
                sum += pho * _decPho
            Else
                MsgBox("Enter a number of Pho Bo")
            End If
        Else
            txtPho.Text = "0"
        End If
        If ChbBxeo.Checked = True Then
            If IsNumeric(txtBxeo.Text) Then
                bxeo = Convert.ToInt32(txtBxeo.Text)
                sum += bxeo * _decBxeo
            Else
                MsgBox("Enter a number of Banh Mi")
            End If
        Else
            txtBxeo.Text = "0"
        End If
        If chbBcha.Checked = True Then
            If IsNumeric(txtBcha.Text) Then
                bcha = Convert.ToInt32(txtBcha.Text)
                sum += bcha * _decBcha
            Else
                MsgBox("Enter a number of Bun Cha")
            End If
        Else
            txtBcha.Text = "0"
        End If
        If chbBbao.Checked = True Then
            If IsNumeric(txtBbao.Text) Then
                bbao = Convert.ToInt32(txtBbao.Text)
                sum += bbao * _decBbao
            Else
                MsgBox("Enter a number of Banh Bao")
            End If
        Else
            txtBbao.Text = "0"
        End If
        If chbCoke.Checked = True Then
            If IsNumeric(txtCoke.Text) Then
                coke = Convert.ToInt32(txtCoke.Text)
                sum += coke * _decCoke
            Else
                MsgBox("Enter a number of Coke")
            End If
        Else
            txtCoke.Text = "0"
        End If
        If chbSprite.Checked = True Then
            If IsNumeric(txtSprite.Text) Then
                sprite = Convert.ToInt32(txtSprite.Text)
                sum += sprite * _decSprite
            Else
                MsgBox("Enter a number of Sprite")
            End If
        Else
            txtSprite.Text = "0"
        End If
        If chbJuice.Checked = True Then
            If IsNumeric(txtJuice.Text) Then
                juice = Convert.ToInt32(txtJuice.Text)
                sum += juice * _decJuice
            Else
                MsgBox("Enter a number of Orange Juice")
            End If
        Else
            txtJuice.Text = "0"
        End If
        If chbTea.Checked = True Then
            If IsNumeric(txtTea.Text) Then
                tea = Convert.ToInt32(txtTea.Text)
                sum += tea * _decTea
            Else
                MsgBox("Enter a number of Sweet Tea")
            End If
        Else
            txtTea.Text = "0"
        End If

        If chbBoba.Checked = True Then
            If IsNumeric(txtBoba.Text) Then
                boba = Convert.ToInt32(txtBoba.Text)
                sum += boba * _decBoba
            Else
                MsgBox("Enter a number of Boba Tea")
            End If
        Else
            txtBoba.Text = "0"
        End If
        If chbCPSD.Checked = True Then
            If IsNumeric(txtCPSD.Text) Then
                cpsd = Convert.ToInt32(txtCPSD.Text)
                sum += cpsd * _decCpsd
            Else
                MsgBox("Enter a number of Ca Phe Sua Da")
            End If
        Else
            txtCPSD.Text = "0"
        End If
        txtPrice.Text = sum.ToString()
    End Sub

    Private Sub chbBmi_CheckedChanged(sender As Object, e As EventArgs) Handles chbBmi.CheckedChanged
        If chbBmi.Checked = True Then
            txtBmi.Visible = True
            txtBmi.Focus()
        End If
    End Sub
    Private Sub chbGcuon_CheckedChanged(sender As Object, e As EventArgs) Handles chbGcuon.CheckedChanged
        If chbGcuon.Checked = True Then
            txtGcuon.Visible = True
            txtGcuon.Focus()
        End If
    End Sub

    Private Sub ChbBxeo_CheckedChanged(sender As Object, e As EventArgs) Handles ChbBxeo.CheckedChanged
        If ChbBxeo.Checked = True Then
            txtBxeo.Visible = True
            txtBxeo.Focus()
        End If
    End Sub

    Private Sub chbPho_CheckedChanged(sender As Object, e As EventArgs) Handles chbPho.CheckedChanged
        If chbPho.Checked = True Then
            txtPho.Visible = True
            txtPho.Focus()
        End If
    End Sub

    Private Sub chbBcha_CheckedChanged(sender As Object, e As EventArgs) Handles chbBcha.CheckedChanged
        If chbBcha.Checked = True Then
            txtBcha.Visible = True
            txtBcha.Focus()
        End If
    End Sub

    Private Sub chbBbao_CheckedChanged(sender As Object, e As EventArgs) Handles chbBbao.CheckedChanged
        If chbBbao.Checked = True Then
            txtBbao.Visible = True
            txtBbao.Focus()
        End If
    End Sub

    Private Sub chbCoke_CheckedChanged(sender As Object, e As EventArgs) Handles chbCoke.CheckedChanged
        If chbCoke.Checked = True Then
            txtCoke.Visible = True
            txtCoke.Focus()
        End If
    End Sub

    Private Sub chbSprite_CheckedChanged(sender As Object, e As EventArgs) Handles chbSprite.CheckedChanged
        If chbSprite.Checked = True Then
            txtSprite.Visible = True
            txtSprite.Focus()
        End If
    End Sub

    Private Sub chbJuice_CheckedChanged(sender As Object, e As EventArgs) Handles chbJuice.CheckedChanged
        If chbJuice.Checked = True Then
            txtJuice.Visible = True
            txtJuice.Focus()
        End If
    End Sub

    Private Sub chbTea_CheckedChanged(sender As Object, e As EventArgs) Handles chbTea.CheckedChanged
        If chbTea.Checked = True Then
            txtTea.Visible = True
            txtTea.Focus()
        End If
    End Sub

    Private Sub chbBoba_CheckedChanged(sender As Object, e As EventArgs) Handles chbBoba.CheckedChanged
        If chbBoba.Checked = True Then
            txtBoba.Visible = True
            txtBoba.Focus()
        End If
    End Sub

    Private Sub chbCPSD_CheckedChanged(sender As Object, e As EventArgs) Handles chbCPSD.CheckedChanged
        If chbCPSD.Checked = True Then
            txtCPSD.Visible = True
            txtCPSD.Focus()
        End If
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        txtBmi.Visible = False
        txtBbao.Visible = False
        txtBcha.Visible = False
        txtBoba.Visible = False
        txtPho.Visible = False
        txtBxeo.Visible = False
        txtGcuon.Visible = False
        txtSprite.Visible = False
        txtJuice.Visible = False
        txtTea.Visible = False
        txtCPSD.Visible = False
        txtCoke.Visible = False
    End Sub
End Class
