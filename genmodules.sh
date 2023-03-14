#!bash
set -euo pipefail

cd hs/I

mkdir -p Generated

automsg="-- File generated from Word8.hs by geninstances.sh. Do not modify."
for i in Word Word16 Word32 Word64 CUChar CUShort CUInt CULong CSize CULLong \
         CUIntPtr CUIntMax; do
  o="Generated/$i.hs"
  echo $o
  echo "$automsg" > $o
  cat Word8.hs >> $o
  sed -i "s/^module I.Word8/module I.Generated.$i/g" $o
  sed -i "s/Word8/$i/g" $o
done

automsg="-- File generated from Int8.hs by geninstances.sh. Do not modify."
for i in Int Int16 Int32 Int64 CChar CSChar CShort CInt CLong CPtrdiff CWchar \
         CLLong CIntPtr CIntMax; do
  o="Generated/$i.hs"
  echo $o
  echo "$automsg" > $o
  cat Int8.hs >> $o
  sed -i "s/^module I.Int8/module I.Generated.$i/g" $o
  sed -i "s/Int8/$i/g" $o
done

