'Breakout

Option explicit
Option continuation lines on
'Option fast audio off

CLS
Timer = 0

Const s_w = 320
Const s_h = 320
Const bg = RGB(0,0,0)
Const true = 1
Const false = 0

Const up = 128
Const down = 129
Const left = 130
Const right = 131
Const space = 32
Const esc = 27

Dim l_down = 0
Dim r_down = 0
Dim space_down = 0

Const p_w = 100
Const p_h = 10
Dim p_x = s_w\2 - p_w\2
Dim p_y = 300
Dim p_dx = 0
Dim p_accx = 0
Const p_maxAccx = 0.38

Dim balls = 3
Const maxTime = 90
Dim countdown = maxTime
Dim score = 0

Sub p_clear
  Box p_x, p_y, p_w, p_h, 1, RGB(black),RGB(black)
End Sub
Sub p_draw
  Box p_x, p_y, p_w, p_h, 1, RGB(red), RGB(red)
End Sub
Sub p_bounce
  If Abs(p_dx) < 1 Then
    p_dx = 0
  Else
    p_dx = -p_dx * 0.4
    Play tone 500, 600, 30
  EndIf
End Sub
Sub p_update
  If l_down Then
    p_accx = -p_maxAccx
  ElseIf r_down Then
    p_accx = p_maxAccx
  Else
    p_accx = -p_dx/10
  EndIf

  p_dx = p_dx + p_accx
  p_x = p_x + p_dx

  If p_x + p_w > s_w Then
    p_x = s_w - p_w
    p_bounce
  ElseIf p_x < 0 Then
    p_x = 0
    p_bounce
  EndIf
End Sub

Dim b_x = s_w \ 2
Dim b_y = p_y - 40
Dim b_dx = -6
Dim b_dy = -6
Dim b_attached = 1
Const b_maxDx = 12
Const b_r = 8
Const b_col = RGB(white)
Sub b_clear
  Circle b_x, b_y, b_r,1,1, bg, bg
End Sub
Sub b_draw
  Circle b_x, b_y, b_r,1,1, b_col, b_col
End Sub
Sub b_update
  If space_down Then
    b_attached = 0
    Play tone 200,300,120
  EndIf

  If b_attached Then
    b_attachMove
  Else
    b_go
  End If
End Sub
Sub b_attachMove
  b_y = p_y - b_r
  b_x = p_x + p_w/4
End Sub
Sub b_go
  ' apply vel
  b_x = b_x + b_dx
  b_y = b_y + b_dy

  ' bounce off walls
  If b_x - b_r < 0 Then
    b_x = b_r
    b_dx = -b_dx
    Play tone 400,500,100
  ElseIf b_x + b_r > s_w Then
    b_x = s_w - b_r
    b_dx = -b_dx
    Play tone 400,500,100
  EndIf

  'collide with bricks
  Local row = brick_rows-1
  Do
    If b_y-b_r < brick_y+row*brick_h + brick_h Then
      If collide_brick(row) Then
        Play tone 700,600,80
        Exit
      EndIf
    EndIf
    row = row - 1
    If row < 0 Then
      Exit
    EndIf
  Loop

  ' bounce off roof
  If b_y - b_r < 20 Then
    b_y = b_r + 20
    b_dy = -b_dy
    Play tone 400,500,100
  ' die or bounce off player
  ElseIf b_y > s_h Then
    die
    Play tone 200,150,350
  ElseIf b_y + b_r > p_y Then
    collide_player
    Play tone 200,300,120
  EndIf
End Sub

Function collide_brick(row)
  Local col = (b_x-brick_x) \ brick_w
  If bricks(col, row) Then
    break_brick col, row
    b_dy = -b_dy
    collide_brick = 1
  Else
    collide_brick = 0
  EndIf
End Function

Sub collide_player
  If p_x < b_x And p_x + p_w > b_x Then
    Local hor_fac = 2*(b_x - p_x-p_w/2)/p_w
    b_dx = b_maxDx * hor_fac
    b_y = p_y - b_r
    b_dy = -b_dy
  EndIf
End Sub

Sub die
  balls = balls - 1
  If balls = 0 Then
    done = 1
  Else
    ' update life display
    draw_hud
  EndIf

  b_attached = 1
  b_dy = -6
  b_dx = -6
End Sub


' BRICKS
Const brick_h = 20
Const brick_w = 60
Const brick_x = 10
Const brick_y = 30
Const brick_cols = 5
Const brick_rows = 6
Dim bricks(brick_cols,brick_rows)
Dim bricksRemaining = brick_cols*brick_rows

Sub init_bricks
  Local r, c
  For r = 0 To brick_rows-1
    For c = 0 To brick_cols-1
      bricks(c,r)=1
    Next c
  Next r
End Sub
init_bricks
Sub break_brick c, r
  bricks(c, r) = 0
  bricksRemaining = bricksRemaining-1
  clear_brick c, r
  score = score + 100*countdown
  If bricksRemaining = 0 Then
    done = 1
  EndIf
  draw_hud
End Sub
Sub draw_bricks
  Local c, r
  For r = 0 To brick_rows-1
   For c = 0 To brick_cols-1
      If bricks(c,r) Then
        clear_brick c, r
        draw_brick c, r
      EndIf
    Next c
  Next r
End Sub
Sub clear_brick c, r
  Box brick_x + c*brick_w, brick_y + r*brick_h, brick_w, brick_h, 2, bg, bg
End Sub
Sub draw_brick c, r
    Static line_col
    Static bg_col
    If (c Mod 2 + r Mod 2)Mod 2 = 0 Then
      line_col = RGB(150, 30, 30)
      bg_col = RGB(210, 110, 110)
    Else
      line_col = RGB(30, 150, 30)
      bg_col = RGB(110, 210, 110)
    EndIf

    Box brick_x + c*brick_w, brick_y + r*brick_h, brick_w, brick_h, 2, line_col, bg_col
End Sub
draw_bricks

Dim pTime = 0
Dim cTime = 0
Dim dTime = 0
Dim remTime = 0
Const targetDt = 1/60

Function clamp(v, min, max)
  If v > max Then
    clamp = max
  ElseIf v < min Then
    clamp = min
  Else
    clamp = v
  EndIf
End Function

' input's a keyword
Sub inp
  space_down = 0

  Local key = Asc(Inkey$)
  'Local num = keydown(0)
  'For i = 1 To num
  '  Print i, num
  '  Local key = keydown(i)
    If key = left Then
      l_down = 1
      r_down = 0
    ElseIf key = right Then
      l_down = 0
      r_down = 1
    ElseIf key = space Then
      space_down = 1
    ElseIf key = esc Then
      Play tone 800, 600, 70
      done = 1
    End If
  'Next i
End Sub

Sub update
  Local oldC = countdown
  countdown = maxTime - Timer \ 1000
  If countdown = 0 Then
    done = 1
  EndIf
  If oldC <> countdown Then
    draw_hud
  EndIf
  p_update
  b_update
End Sub

Sub cl
  p_clear
  b_clear
End Sub
Sub draw
  p_draw
  b_draw
  'draw_bricks
  'draw_hud
End Sub
Sub draw_hud
  Static timeStr$ = "Time: "
  If countdown = 9 Then
    timeStr$ = timeStr$ + " "
  EndIf

  Text 10, 10-3, timeStr$ + Str$(countdown)
  Text s_w/2, 10-3, "Score: " + Str$(score), "C"
  Local i
  For i = 1 To 5
    Circle s_w - 20*i, 10, 6, 1, 1, bg, bg
  If i < balls Then
      Circle s_w - 20*i, 10, 6, 1, 1, RGB(white), RGB(white)
    EndIf
  Next i
End Sub

Dim done = 0
game_loop:
done = 0
draw_hud
Do While Not done
  pTime = cTime
  cTime = Timer
  dTime = cTime - pTime

  cl
  inp
  update
  draw

  remTime = dTime - (Timer - cTime)
  remTime = clamp(remTime, 0.00000001, targetDt)
  CPU sleep remTime
Loop

done = 0
If bricksRemaining = 0 Then
  score = score + countdown * 1500
  score = score + balls * 20000
EndIf
CLS
menu
Sub menu
  draw_menu
  Local key = 0
  Local index = 0
  Local oldIndex = 0
  Do
    key = Asc(Inkey$)
    Select Case key
      Case 0
        Continue do
      Case space
        Play tone 600,400,200
        If index = 0 Then
          reset
          ' fall out to the goto
          Exit
        ElseIf index = 1 Then
          ' jump out
          GoTo e
        EndIf
      Case up
        Play tone 800, 600, 70
        oldIndex = index
        index = clamp(index-1, 0, 1)
      Case down
        Play tone 600, 800, 70
        oldIndex = index
        index = clamp(index+1, 0, 1)
    End Select
    ' redraw the > (and everything else)
    draw_menu index, oldIndex
  Loop
End Sub
Sub draw_menu index, oldIndex
  If bricksRemaining = 0 Then
    'time bonus
    Text 70, 40, "Time bonus:  "+ Str$(countdown) + " seconds"
    Text 70, 55, "             " +Str$(countdown * 1000) + " points"
    Text 70, 80, "Balls bonus: " +Str$(balls)+" balls"
    Text 70, 95, "             "+Str$(balls* 15000) +" points"

  Else
    Text 70, 40," No time bonus"
    Text 70, 80, " No balls bonus"
  EndIf

  Text 70, 120, "Total score: "+ Str$(score)

  ' clear old >
  Text 100, 180 + 20*oldIndex, ">",, , ,bg
  Text 100, 180 + 20*index, ">"
  Text 110, 180, "Play again"
  Text 110, 200, "Quit"
End Sub
Sub reset
  CLS
  Timer = 0
  p_x = s_w\2 - p_w\2
  p_y = 300
  p_dx = 0
  p_accx = 0
  countdown = maxTime
  score = 0
  balls = 3
  b_x = s_w \ 2
  b_y = p_y - 40
  b_dx = -6
  b_dy = -6
  b_attached = 1
  bricksRemaining = brick_cols*brick_rows
  init_bricks
  draw_bricks
  pTime = 0
  cTime = 0
  dTime = 0
  remTime = 0
  l_down = 0
  r_down = 0
End Sub
GoTo game_loop
e:
CLS
Print "Thanks for playing <3"
Print
