@echo off
start shutdown.exe -s -t 60 -c "DPGエンコードが終了したので、自動シャットダウンをスケジュールしました。シャットダウンを中止するときは、DOSプロンプト（背後の黒いウィンドウ）をクリックしてから何かキーを押してください。" -f
echo _
echo シャットダウンを中止するときは、このウィンドウをクリックしてから何かキーを押してください。
echo _
pause
start shutdown.exe -a
