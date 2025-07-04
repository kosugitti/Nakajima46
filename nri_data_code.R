rm(list = ls())
urls <- c(
  "https://www.dropbox.com/scl/fi/lc7kuhx4pwg9envyqwxu3/viWEB.Rdata?rlkey=hieiumunurl9vi3o67q110cdv&st=c84sh7p8&dl=1",
  "https://www.dropbox.com/scl/fi/xf75b4pr19bn90tiyqphn/vi.Rdata?rlkey=14ceggesh0lvak4665p0ew6lz&st=wpqi3ulu&dl=1",
  "https://www.dropbox.com/scl/fi/q4svksqyx52mzfv5nuyn5/vi.Rdata?rlkey=eqgiwvz5vpv9xzf5boinycva5&st=eroxrkyb&dl=1",
  "https://www.dropbox.com/scl/fi/eferzl3o86xplun2f836y/vi.Rdata?rlkey=i0f2e8jhyy7cwdmxhx0wglms3&st=ko85lcmk&dl=1",
  "https://www.dropbox.com/scl/fi/wjn5lgxn5nhcgu6s8goqg/WEB-_2025.Rdata?rlkey=pq2s2ywanntoyxggfmgt7cx1d&st=1q2vm3jf&dl=1",
  "https://www.dropbox.com/scl/fi/zpschfod14il0h0zv5s0w/CM-_2025.Rdata?rlkey=usn6v7mgh7xgyrmnerru1y4kw&st=8u49mowz&dl=1",
  "https://www.dropbox.com/scl/fi/vznpi7s5dvgnailtk0ikj/_2025.Rdata?rlkey=9dg4n84hj6qmejgiimv5ird6t&st=l0ye3egu&dl=1",
  "https://www.dropbox.com/scl/fi/cuy9d7x0evbf8r51zon4p/_2025.Rdata?rlkey=bqg6gsu7pczo1y6lgrdjmapuh&st=b9w5k6ak&dl=1",
  "https://www.dropbox.com/scl/fi/du91zzb02agr0bpvgej64/_2025.Rdata?rlkey=6dqtx4z6apmnb0bochjndapzi&st=kzwa8uvr&dl=1"
)

n <- length(urls)
for (i in 1:n) {
  tmp <- tempfile(fileext = ".Rdata")
  download.file(urls[i],
    destfile = tmp, mode = "wb"
  )
  load(tmp)
}
rm(i, n, tmp, urls)
