sampletext=`cat /dev/random | LC_CTYPE=C tr -dc "[:alpha:]" | head -c 62`
echo Sample text = $sampletext
./create_qrcode.sh "$sampletext"
decodetext=`node decodeqr.js qr.png`
if [[ "$sampletext" == "$decodetext" ]]
then
    echo Success
else
    echo Failed "$sampletext" != "$decodetext"
fi

