mkdir -p results/no results/xn
HC=/opt/exp/ghc/ghc-gc/_build/stage1/bin/ghc

run_it() {
  local name="$1"
  local args="$2"
  local root=results/$name
  mkdir -p $root
  cabal new-run edit-dist -w $HC --allow-newer -- \
    $args +RTS -s$root/stats -l -ol$root/run.eventlog > $root/stdout
  nix run -f ../../utils -c dump-samples "req" $root/run.eventlog > $root/samples
}

for i in 50 60 70 80 90 100; do
  run_it "xn-$i" "$i +RTS -xn -RTS"
  run_it "no-$i" "$i"
done

