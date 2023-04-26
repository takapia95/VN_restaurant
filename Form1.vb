Public Class Form1
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles btnCalculate.Click
        Dim sum As Decimal
        If radBmi.Checked Then
            sum += 10D
        ElseIf radGcuon.Checked Then
            sum += 10D
        ElseIf radPho.Checked Then
            sum += 15D
        ElseIf radBcha.Checked Then
            sum += 15D
        ElseIf radBbao.Checked Then
            sum += 15D
        ElseIf radBxeo.Checked Then
            sum += 12D
        End If
        If radCoke.Checked Then
            sum += 2.5D
        ElseIf radSp.Checked Then
            sum += 3D
        ElseIf radJuice.Checked Then
            sum += 3.5D
        ElseIf radSw.Checked Then
            sum += 4D
        ElseIf radBoba.Checked Then
            sum += 4.5D
        ElseIf radCPSD.Checked Then
            sum += 5D
        End If
        txtPrice.Text = sum.ToString()
    End Sub

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
        radBmi.Checked = False
        radGcuon.Checked = False
        radPho.Checked = False
        radBcha.Checked = False
        radBbao.Checked = False
        radBxeo.Checked = False
        radCoke.Checked = False
        radSp.Checked = False
        radJuice.Checked = False
        radSw.Checked = False
        radBoba.Checked = False
        radCPSD.Checked = False
        txtName.Clear()
        txtPark.Clear()
        txtPark.Focus()
        txtPrice.Clear()
        txtTotal.Clear()
        cboTip.SelectedIndex = -1
        cboPayment.SelectedIndex = -1


    End Sub

    Private Sub btnExit_Click(sender As Object, e As EventArgs) Handles btnExit.Click
        Close()
    End Sub
End Class
