#!bash
set -x -euo pipefail

pushd hs/I

automsg="-- File generated from Word8.hs by geninstances.sh. Do not modify."

echo "$automsg" > Word16.hs
cat Word8.hs >> Word16.hs
sed -i "s/Word8/Word16/g" Word16.hs
sed -i "s/255/65535/g" Word16.hs

echo "$automsg" > Word32.hs
cat Word8.hs >> Word32.hs
sed -i "s/Word8/Word32/g" Word32.hs
sed -i "s/255/4294967295/g" Word32.hs

echo "$automsg" > Word64.hs
cat Word8.hs >> Word64.hs
sed -i "s/Word8/Word64/g" Word64.hs
sed -i "s/255/18446744073709551615/g" Word64.hs

popd
