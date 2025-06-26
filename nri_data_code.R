vi_file <- tempfile(fileext = ".Rdata")
download.file("https://www.dropbox.com/scl/fi/gd4ucwnvm10oaar9l1vg7/vi.Rdata?rlkey=iiemu0oo3suiapfq1s02rvues&st=3xvhvtx1&dl=1",
destfile = vi,mode = "wb")
load(vi_file)

main_file <- tempfile(fileext = ".Rdata")
download.file("https://www.dropbox.com/scl/fi/go0gcbuvss6c0witj2vqz/_2022.Rdata?rlkey=p77f0ykl00zioh7rdl0n0oo47&st=lls8z40z&dl=1",
destfile = main_file,mode = "wb")
load(main_file)
