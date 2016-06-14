notification :libnotify

guard :shell do
  path = ".stack-work/install/x86_64-linux/lts-6.2/7.10.3/bin/pgn-vizu"
  watch(path) do
    puts "Checking PGNs:      "
    success = system("stack exec pgn-vizu check examples/*.pgn")
    exit_code = $?.exitstatus
    if success
      n "All PGN checked"
    else
      n "#{exit_code} errors"
    end
  end
end
