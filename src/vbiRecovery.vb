'//============================================================================
'// Name        : vbiRecovery.vb
'// Author       : fallensn0w
'// Version      : 2.0!!!!!!!
'// Copyright   : http://www.fallensn0w-devkit.blogspot.com
'// Description : A VB.NET implementation of "iRecovery 2.X"
'//============================================================================

Imports System.IO

Module vbiRecovery

    Dim iDev As New MobileDevice( _
                                                   MobileDevice.DeviceMode.Recovery _
                                                   )

    Dim iAbout As String = vbCrLf & _
                                       " vbiRecovery -- a utility for iDevices iBSS/iBoot." & vbCrLf & _
                                       " Ported from C to the .NET Framework. By: Fallensn0w" & vbCrLf

    Sub Main()
        If Command() <> "" Then
            Dim xCMD As String = Replace(Command, """", "")

            Console.WriteLine(iAbout)

            If iDev.Connect() = False Then
                Console.Clear()
                Console.WriteLine(iAbout & vbCrLf & _
                                  " No iPhone/iPod found.")
                End
            End If

            If InStr(xCMD, "-c ") Then
                xCMD = Replace(xCMD, "-c ", "")
                iDev.SendCommand(xCMD)

            ElseIf InStr(xCMD, "-b") Then
                xCMD = Replace(xCMD, "-b", "") : xCMD = xCMD.Replace(" ", "")
                If xCMD = "false" Then iDev.AutoBoot(False)
                iDev.AutoBoot(True)

            ElseIf InStr(xCMD, "-x ") Then
                Dim batch As String = Replace(xCMD, "-x ", "")
                Dim batchText As String = GetFileContents(batch)
                Dim batchLine() As String = Split(batchText, vbCrLf)

                For i = 0 To UBound(batchLine)
                    If batchLine(i) = "" Then
                        ' do nothing, it's probably a CrLf no need for doing stuff
                    ElseIf InStr(batchLine(i), "/sleep ") Then
                        batchLine(i) = Replace(batchLine(i), "/sleep ", "")
                        For sleep = 0 To Val(batchLine(i)) * 100
                            Application.DoEvents()
                        Next

                    Else
                        iDev.SendCommand(batchLine(i))
                        WriteLog("Sent Command '" & batchLine(i) & "'")
                    End If
                Next
                WriteLog("Done Sending Commands From Batch File. (" & batch & ")")

            ElseIf InStr(xCMD, "-s") Then

                While (True)

                    Console.Write("$ ")
                    tempCMD = Console.ReadLine()

                    If tempCMD = "/exit" Then
                        prompt = 1

                    ElseIf tempCMD = "/sleep " Then
                        tempCMD = Replace(tempCMD, "/sleep ", "")
                        For sleep = 0 To Val(tempCMD) * 100
                            Application.DoEvents()
                        Next

                    ElseIf InStr(tempCMD, "/autoboot") Then
                        tempCMD = Replace(tempCMD, "/autoboot", "") : tempCMD = Replace(tempCMD, " ", "")
                        If tempCMD = "false" Then iDev.AutoBoot(False)
                        iDev.AutoBoot(True)

                    Else ' if its just a normal command.
                        iDev.SendCommand(tempCMD)
                    End If

                End While

            ElseIf InStr(xCMD, "-f ") Then
                xCMD = Replace(xCMD, "-f ", "")
                iDev.SendFile(xCMD)

            ElseIf InStr(xCMD, "0xA1 ") Then
                xCMD = Replace(xCMD, "0xA1 ", "") : iDev.SendRawUsb_0xA1(xCMD)

            ElseIf InStr(xCMD, "0x40 ") Then
                xCMD = Replace(xCMD, "0x40 ", "") : iDev.SendRawUsb_0x40(xCMD)

            ElseIf InStr(xCMD, "0x21 ") Then
                xCMD = Replace(xCMD, "0x21 ", "") : iDev.SendRawUsb_0x21(xCMD)

            ElseIf (InStr(xCMD, "-arm7")) Then
                iDev.Send_Arm7_Go()

            ElseIf (InStr(xCMD, "-k ")) Then
                xCMD = Replace(xCMD, "-k ", "")
                iDev.SendPayload(xCMD)

            Else
                Console.WriteLine(" Error while executing arguement: '" & xCMD & "'")
                Console.ReadLine()
                End

            End If

            ' releases our connection
            iDev.Dispose()

            ' Console.Write(vbCrLf & " Press any key to exit . . .")
            'Console.Read()


        Else

            Console.WriteLine(iAbout & vbCrLf & _
                                        "       ./vbiRecovery.exe -c <cmd>         sends a stand-alone command." & vbCrLf & _
                                        "       ./vbiRecovery.exe -b               this will send the commands needed for recovery loop fix." & vbCrLf & _
                                        "       ./vbiRecovery.exe -k <file>        sends your payload + exploit." & vbCrLf & _
                                        "       ./vbiRecovery.exe -x <file>        runs batch commands from a file (one per line)." & vbCrLf & _
                                        "       ./vbiRecovery.exe -f <file>        uploads a file." & vbCrLf & vbCrLf & _
                                        vbCrLf _
                                        )

        End If

    End Sub


    Public Function GetFileContents(ByVal FullPath As String, Optional ByRef ErrInfo As String = "") As String
        Dim strContents As String = "" : Dim objReader As StreamReader : Try : objReader = New StreamReader(FullPath) : strContents = objReader.ReadToEnd() : objReader.Close() : Catch Ex As Exception : ErrInfo = Ex.Message : End Try : Return strContents
    End Function


    Public Sub WriteLog(ByVal Text As String)
        old = GetFileContents("MobileDevice.txt")
        Console.WriteLine(" MobileDevice" & " @ " & TimeOfDay & " -> " & LCase(Text))
        'SaveTextToFile((old & "MobileDevice" & " @ " & TimeOfDay & " -> " & LCase(Text)) & vbCrLf, "MobileDevice.txt")
    End Sub

End Module