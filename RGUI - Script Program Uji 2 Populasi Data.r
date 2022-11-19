#------------------------------------------------------------------------tampilan awal
require(tcltk)
jdlu=tktoplevel(bg="burlywood")
tktitle(jdlu)="MENU UTAMA PROGRAM"
topmenu=tkmenu(jdlu)
submenu=tkmenu(jdlu)
subsubmenu<-tkmenu(jdlu)
tkconfigure(jdlu,menu=topmenu)


end=function()
{
tkdestroy(jdlu)
tkmessageBox(message=paste("Terimakasih sudah menggunakan program ini :)"))
}

font1=tkfont.create(family="courier",size=12)
font2=tkfont.create(family="sans",size=20,weight="bold")
font3=tkfont.create(family="times",size=18,weight="bold")
font4=tkfont.create(family="times",size=16)

spec.frm<-tkframe(jendela,borderwidth=2)
frame1<-tkframe(jdlu,relief="sunken",borderwidth=30,bg="coral")
frame2<-tkframe(jdlu,relief="raised",borderwidth=30,bg="lightblue")
frame3<-tkframe(jdlu,relief="solid",borderwidth=10,bg="lightsalmon")

tkpack(tklabel(frame1,text="SELAMAT DATANG DALAM PROGRAM KAMI",font=font2,bg="coral"))
tkpack(tklabel(frame1,text="STATISTIKA KOMPUTASI",font=font2,bg="coral"))
tkpack(tklabel(frame1,text="UJI RATA - RATA 2 POPULASI (PARAMETRIK & NON PARAMETRIK)",font=font2,bg="coral"))
tkpack(frame1,fill="both")

tkpack(tklabel(frame2,text=" ",bg="lightblue"))
tkpack(tklabel(frame2,text="SILAHKAN GUNAKAN MENU DAN KOTAK INPUTAN YANG SUDAH DISEDIAKAN UNTUK MULAI ANALISIS",font=font4,bg="lightblue"))
tkpack(tklabel(frame2,text="ANDA DAPAT MENGIMPORT DATA DARI SPSS ATAU MENGINPUT DATA SENDIRI",font=font4,bg="lightblue"))
tkpack(frame2,fill="both")

tkpack(tklabel(frame2,text=" ",bg="lightblue"))
tkpack(tklabel(frame2,text="Silahkan pilih menu 'INFO' untuk mengetahui materi uji dan cara menggunakan program ini",font=font4,bg="lightblue"))
tkpack(tklabel(frame2,text=" ",bg="lightblue"))
tkpack(frame2,fill="both")

tkpack(tklabel(frame3,text="MAHASISWA KELAS S2",font=font2,bg="lightsalmon"))
tkpack(tklabel(frame3,text="DIBUAT OLEH :",font=font1,bg="lightsalmon"))
tkpack(tklabel(frame3,text="1. Salsabylla Nada Apsariny (081811833027",font=font1,bg="lightsalmon"))

tkpack(tklabel(frame3,text="
PROGRAM STUDI S1 STATISTIKA
FAKULTAS SAINS DAN TEKNOLOGI
UNIVERSITAS AIRLANGGA
SURABAYA
2020
",font=font4,bg="lightsalmon"))
tkpack(frame3,fill="both")

tkpack(frame1,fill="x")
tkpack(frame2,fill="y")
tkpack(frame3,fill="y")

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

eb=tkbutton(jdlu, text="KELUAR",command=end,bg="darkturquoise")
tkpack(eb, padx = 5, pady = 5)

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx





#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx UJI Z DATA RINGKASAN XXXXXXXXXXXXXXXXXXXXXX
asumsi1ringkasan=function()
{
require(tcltk)
jdlr1=tktoplevel()
tktitle(jdlr1)<-"Data Ringkasan Uji Z Variansi Populasi Diketahui"

teksaa=tkfont.create(family="sans",weight="bold",size=10)
teksbb=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jdlr1,text="UJI RATA RATA 2 SAMPEL BEBAS",font=teksbb),sticky="n")
tkgrid(tklabel(jdlr1,text="ASUMSI KE 1 (DATA RINGKASAN)",font=teksbb),sticky="n")
tkgrid(tklabel(jdlr1,text=" "))

hip0=tclVar("Miu1 - Miu2 = d0")
h0=tkentry(jdlr1,width="100",textvariable=hip0)
tkgrid(tklabel(jdlr1,text="H0:",font=teksaa))
tkgrid(h0)

rb1=tkradiobutton(jdlr1)
rb2=tkradiobutton(jdlr1)
rb3=tkradiobutton(jdlr1)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jdlr1,text="Pilihan hipotesis:",font=teksaa))
tkgrid(tklabel(jdlr1,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jdlr1,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jdlr1,text="H1 : Miu1 - Miu2  > d0"),rb3)

 tkgrid(tklabel(jdlr1,text=" "))
 nsatu<-tclVar("0")
 eb11=tkentry(jdlr1,width="5",textvariable=nsatu)
 tkgrid(tklabel(jdlr1,text="Banyak Sampel Pertama :"))		 							
 tkgrid(eb11)

 ndua=tclVar("0")
 eb22=tkentry(jdlr1,width="5",textvariable=ndua)
 tkgrid(tklabel(jdlr1,text="Banyak Sampel Kedua:"))
 tkgrid(eb22)

dval=tclVar("0")
dent=tkentry(jdlr1,width="5",textvariable=dval)
tkgrid(tklabel(jdlr1,text="Nilai d0 (d nol) :"))
tkgrid(dent)

slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jdlr1,text=as.numeric(tclvalue(slidervalue)))
tkgrid(tklabel(jdlr1,text="Nilai alfa :"),slidervaluelabel,tklabel(jdlr1,text="%"))
tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jdlr1,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")
tkgrid(alfa)

#----------------------------------------------------------fungsiinputdataringkas1
fungsitabel1=function()
{
n1=as.numeric(tclvalue(nsatu))
n2=as.numeric(tclvalue(ndua))

 xb11=tclVar("0")
 xbe1=tkentry(jdlr1,width="5",textvariable=xb11)
 tkgrid(tklabel(jdlr1,text="Rata - Rata Sampel Pertama :"))
 tkgrid(xbe1)

 xb21=tclVar("0")
 xbe2=tkentry(jdlr1,width="5",textvariable=xb21)
 tkgrid(tklabel(jdlr1,text="Rata - Rata Sampel Kedua :"))
 tkgrid(xbe2)

 sd11=tclVar("0")
 sde1=tkentry(jdlr1,width="5",textvariable=sd11)
 tkgrid(tklabel(jdlr1,text="Simpangan Baku Sampel Pertama :"))
 tkgrid(sde1)

 sd21=tclVar("0")
 sde2=tkentry(jdlr1,width="5",textvariable=sd21)
 tkgrid(tklabel(jdlr1,text="Simpangan Baku Sampel Kedua :"))
 tkgrid(sde2)

#----------------------------------------------------------program uji Z
inputdr1=function()
{
 a=as.numeric(tclvalue(slidervalue))
 n1=as.numeric(tclvalue(nsatu))
 n2=as.numeric(tclvalue(ndua))
 xb1=as.numeric(tclvalue(xb11))
 xb2=as.numeric(tclvalue(xb21))
 sd1=as.numeric(tclvalue(sd11))
 sd2=as.numeric(tclvalue(sd21))
 dnol=as.numeric(tclvalue(dval))
 alfa=a/100
 z=(xb1-xb2-dnol)/sqrt(((sd1^2)/n1)+((sd2^2)/n2))

#------------------------------------------------------------jendela (hasil)
require(tcltk)
jdlr11 <- tktoplevel()
tkwm.title(jdlr11, "HASIL ANALISIS UJI Z VARIANSI DIKETAHUI")

hasil0=function()
{
teks11=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jdlr11,text=" "))
tkgrid(tklabel(jdlr11,text="HASIL ANALISIS UJI RATA-RATA POPULASI 2 SAMPEL BEBAS:",font=teks11))
tkgrid(tklabel(jdlr11,text="ASUMSI 1 : VARIANSI DIKETAHUI(DATA RINGKASAN)",font=teks11))
tkgrid(tklabel(jdlr11,text=" "))

tkgrid(tklabel(jdlr11,text="HIPOTESIS"))
teks <- tktext(jdlr11,height="3")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0 : Miu1 - Miu2 =  ")
tkinsert(teks,"end",dnol)
tkinsert(teks,"end","\n\n")

rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks,"end","H1 : Miu1 - Miu2 != ") 
  tkinsert(teks,"end",dnol)
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks,"end","H1 : Miu1 - Miu2 <  ")
  tkinsert(teks,"end",dnol)
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks,"end","H1 : Miu1 - Miu2 >  ")
  tkinsert(teks,"end",dnol)
 }


tkgrid(tklabel(jdlr11,text="\nDATA RINGKASAN"))
teks2<- tktext(jdlr11,height="12")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel ke-1 :                       ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel ke-2 :                       ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Rata - rata sampel ke-1 :                  ")
tkinsert(teks2,"end",xb1)
tkinsert(teks2,"end","\nRata - rata sampel ke-2 :                  ")
tkinsert(teks2,"end",xb2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Simp.Baku sampel ke-1 :                    ")
tkinsert(teks2,"end",sd1)
tkinsert(teks2,"end","\nSimp.Baku sampel ke-2 :                    ")
tkinsert(teks2,"end",sd2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai d nol (d0):                        ")
tkinsert(teks2,"end",dnol)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jdlr11,text="\nDAERAH KRITIS"))
teks3<- tktext(jdlr11,height="2")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung < -(Z-tabel) atau Z-hitung > Z-tabel") 
  zt=qnorm(1-alfa/2,0,1)
  if(z>-zt && z<zt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung < -(Z-tabel)")
  zt=qnorm(1-alfa,0,1)
  if(z>-zt)a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung > Z-tabel")
  zt=qnorm(1-alfa,0,1)
  if(z<zt) a=0
 }

tkgrid(tklabel(jdlr11,text="\nSTATISTIK UJI"))
teks4<- tktext(jdlr11,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:                   ")
tkinsert(teks4,"end",z)
tkinsert(teks4,"end","\n\n")
tkinsert(teks4,"end","Daerah Kritis	  :                   ")
tkinsert(teks4,"end",zt)

tkgrid(tklabel(jdlr11,text="\nHASIL"))
teks5<- tktext(jdlr11,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (a==0)
{
tkinsert(teks5,"end","Keputusan : Terima H0\n\nKesimpulan : Miu1 - Miu2 = ")
tkinsert(teks5,"end",dnol)
}
else
{
 rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks5,"end","Keputusan: Tolak H0\n\nKesimpulan: Miu1 - Miu2 != ")
  tkinsert(teks5,"end",dnol)
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks5,"end","Keputusan: Tolak H0\n\nKesimpulan: Miu1 - Miu2 < ")
  tkinsert(teks5,"end",dnol)
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks5,"end","Keputusan: Tolak H0\n\nKesimpulan: Miu1 - Miu2 > ")
  tkinsert(teks5,"end",dnol)
 }
}

tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombolend<-tkbutton(jdlr11,text="SELESAI",command=function()tkdestroy(jdlr11),bg="aquamarine")
tkgrid(tombolend)
}
hasil0()
}

tkdestroy(tmblend)

tombolprint<-tkbutton(jdlr1,text="HASIL ANALISIS",command=inputdr1,bg="gold")
tkgrid(tombolprint,padx=5,pady=5)


balik=tkbutton(jdlr1,text="KEMBALI KE MENU",command=function()tkdestroy(jdlr1),bg="turquoise")
tkgrid(balik,sticky="n",padx=5,pady=5)

}#--------------------------------------------------------------------inputdr

tombolnext<-tkbutton(jdlr1,text="MASUKKAN DATA",command=fungsitabel1,bg="skyblue")
tkgrid(tombolnext,padx=5,pady=5)

tmblend<-tkbutton(jdlr1,text="KEMBALI KE MENU",command=function()tkdestroy(jdlr1),bg="turquoise")
tkgrid(tmblend,padx=5,pady=5,sticky="n")
}

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx




#xxxxxxxxxxxxxxxxxxxxxxxxxxxx  UJI Z DATA INPUT  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#----------------------------------------jendela input data uji z asumsi-1

asumsi1input=function()
{
require(tcltk)
jendela=tktoplevel()
tktitle(jendela)<-"Data Inputan Uji Z Variansi Populasi Diketahui"

teksa=tkfont.create(family="sans",weight="bold",size=10)
teksb=tkfont.create(family="sans",weight="bold",size=14)

tkgrid(tklabel(jendela,text="UJI RATA RATA 2 SAMPEL BEBAS",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text="ASUMSI KE 1 (INPUT DATA)",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text=" "))

hip0=tclVar(" ")
h0=tkentry(jendela,width="100",textvariable=hip0)
tkgrid(tklabel(jendela,text="H0:",font=teksa))
tkgrid(h0)

tkgrid(tklabel(jendela,text=" "))

rb1=tkradiobutton(jendela)
rb2=tkradiobutton(jendela)
rb3=tkradiobutton(jendela)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jendela,text="Pilihan hipotesis:",font=teksa))
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2  > d0"),rb3)

tkgrid(tklabel(jendela,text=" "))

n11=tclVar("0")
eb1=tkentry(jendela,width="5",textvariable=n11)
tkgrid(tklabel(jendela,text="Banyak Sampel Pertama:"))
tkgrid(eb1)

n22=tclVar("0")
eb2=tkentry(jendela,width="5",textvariable=n22)
tkgrid(tklabel(jendela,text="Banyak Sampel Kedua:"))
tkgrid(eb2)

dval=tclVar("0")
dent=tkentry(jendela,width="5",textvariable=dval)
tkgrid(tklabel(jendela,text="Nilai d0 (d nol) :"))
tkgrid(dent)

#----------------------------------------------------------fungsitabel1

fungsitabel1=function()
{
n1=as.numeric(tclvalue(n11))
n2=as.numeric(tclvalue(n22))

#------------------------------------------------------------------if

 if(n1==0||n2==0)
  {tkmessageBox(message="ANDA BELUM MENGINPUT BANYAK DATA DENGAN BENAR",icon="warning")}
  else
  {

#-------------------------------------------------------------jendela1(tampilan ketika data diinput)

require(tcltk)
jendela1=tktoplevel()
tktitle(jendela1)<-"Input Sampel"


teks1=tkfont.create(family="sans",weight="bold",size=14)
	tkgrid(tklabel(jendela1,text="MASUKKAN DATA",font=teks1))
	tkgrid(tklabel(jendela1,text=" "))
	tkgrid(tklabel(jendela1,text=" "))
tkgrid(tklabel(jendela1,text="Sampel pertama :                   Sampel kedua :",font=teks1),sticky="n")

#----------------------------------------------------------------input tabel1

tclRequire("Tktable")
bar1=as.numeric(tclvalue(n11))
kol=1
wadah1=matrix(0,bar1,kol)
x<-matrix(0,bar1,kol)
isitabel1= tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-wadah1[i,j]
 }
}
isitabel1[[0,0]]="n1"

tabel1=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel1=tkwidget(jendela1,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")

#----------------------------------------------------------------input tabel2

bar2=as.numeric(tclvalue(n22))
kol=1
wadah2=matrix(0,bar2,kol)
y<-matrix(0,bar2,kol)
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-wadah2[i,j]
 }
}
isitabel2[[0,0]]="n2"

tabel2=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel2=tkwidget(jendela1,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jendela,text=as.numeric(tclvalue(slidervalue)))

tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jendela,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")

#........................Menampilkan Data yang sudah diinput..........................

periksa=function()
{
require(tcltk)
jendela00=tktoplevel()
tktitle(jendela00)="Input Sampel"
tclRequire("Tktable")

for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

bar1=as.numeric(tclvalue(n11))
kol=1
isitabel1=tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


bar2=as.numeric(tclvalue(n22))
kol=1
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2=tkwidget(jendela00,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup=tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}

input1=function()    #print nilai fungsi n1
{
 for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

#--------------------------------------------------------program uji z
n1=length(x)
n2=length(y)
a=as.numeric(tclvalue(slidervalue))
dnol=as.numeric(tclvalue(dval))
alfa=a/100
z=(mean(x)-mean(y)-dnol)/sqrt((var(x)/n1)+(var(y)/n2))

#----------------------------------------------------------jendela2(hasil)
require(tcltk)
jendela2=tktoplevel()
tkwm.title(jendela2, "HASIL ANALISIS UJI Z VARIANSI DIKETAHUI")

hasil=function()
{
tkgrid(tklabel(jendela2,text=" "))
tkgrid(tklabel(jendela2,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BEBAS :",font=teks1))
tkgrid(tklabel(jendela2,text="ASUMSI 1 : VARIANSI DIKETAHUI(INPUT DATA)",font=teks1))
tkgrid(tklabel(jendela2,text=" "))

tkgrid(tklabel(jendela2,text="HIPOTESIS"))
teks=tktext(jendela2,height="3")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0 : Miu1 - Miu2 =  ")
tkinsert(teks,"end",dnol)
tkinsert(teks,"end","\n\n")

rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks,"end","H1 : Miu1 - Miu2 != ") 
  tkinsert(teks,"end",dnol)
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks,"end","H1 : Miu1 - Miu2 <  ")
  tkinsert(teks,"end",dnol)
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks,"end","H1 : Miu1 - Miu2 >  ")
  tkinsert(teks,"end",dnol)
 }

tkgrid(tklabel(jendela2,text="\nDATA INPUTAN"))
teks2=tktext(jendela2,height="7")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela2,text="\nDAERAH KRITIS"))
teks3=tktext(jendela2,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung < -(Z-tabel) atau Z-hitung > Z-tabel") 
  zt=qnorm(1-alfa/2,0,1)
  if(z>-zt || z<zt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung < -(Z-tabel)")
  zt=qnorm(1-alfa,0,1)
  if(z>-zt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung > Z-tabel")
  zt=qnorm(1-alfa,0,1)
  if(z<zt) a=0
 } 


tkgrid(tklabel(jendela2,text="\nSTATISTIK UJI"))
teks4=tktext(jendela2,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:\n                    ")
tkinsert(teks4,"end",z)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Statistik tabel:\n                    ")
tkinsert(teks4,"end",zt)

tkgrid(tklabel(jendela2,text="\nHASIL"))
teks5<- tktext(jendela2,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)


if (a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Terima H1\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}

tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombolse<-tkbutton(jendela2,text="SELESAI",command=function()tkdestroy(jendela2),bg="aquamarine")
tkgrid(tombolse,sticky="n")
tkgrid(tklabel(jendela2,text=" "))

}
hasil()
}

tkdestroy(tmblselesai)


tombolperiksa<-tkbutton(jendela,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tkgrid(tklabel(jendela,text="Nilai alfa:"),slidervaluelabel,tklabel(jendela,text="%"))
tkgrid(alfa)

tombolprint<-tkbutton(jendela,text="HASIL ANALISIS",command=input1,bg="gold")
tkgrid(tombolprint,padx=5,pady=5)

selesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(selesai,padx=5,pady=5,sticky="n")

tombolselesai1<-tkbutton(jendela1,text="SELESAI",command=function()tkdestroy(jendela1),bg="aquamarine")
tkgrid(tombolselesai1,sticky="n")
}
} #-----------------------------------------------------tutup if
} #-----------------------------------------------------tutup fungsi tabel 1


tkgrid(tklabel(jendela,text=" "))
tombolnext<-tkbutton(jendela,text="MASUKKAN DATA",command=fungsitabel1,bg="skyblue")
tkgrid(tombolnext,padx=5,pady=5)

tmblselesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(tmblselesai,padx=5,pady=5,sticky="n")

}

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx   UJI Z DATA IMPORT   xxxxxxxxxxxxxxxxxxxxxxxxxxxx
asumsi1import=function()
{

require(tcltk)
jdl3=tktoplevel()
tktitle(jdl3)="Data Import SPSS Uji Z Variansi Populasi Diketahui"
  

font1=tkfont.create(family="sans",size=14,weight="bold")
font2=tkfont.create(family="times",size=10,weight="bold")
tkgrid(tklabel(jdl3,text="UJI 2 RATA RATA SAMPEL BEBAS",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text="ASUMSI KE 1 (DATA IMPORT)",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text=" "))


hip0=tclVar(" ")
h0=tkentry(jdl3,width="100",textvariable=hip0) 
tkgrid(tklabel(jdl3,text="H0:",font=font2)) 
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jdl3,width="100",textvariable=hip1)
tkgrid(tklabel(jdl3,text="H1:",font=font2))
tkgrid(h1) 
teks3=tclVar()

tkgrid(tklabel(jdl3,text=" "))

rb1=tkradiobutton(jdl3)
rb2=tkradiobutton(jdl3)
rb3=tkradiobutton(jdl3)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jdl3,text="Pilihan hipotesis:",font=font2))
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2  > d0"),rb3)
                
tkgrid(tklabel(jdl3,text=" "))
slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jdl3,text=as.numeric(tclvalue(slidervalue)))
tkgrid(tklabel(jdl3,text="Nilai alfa:"),slidervaluelabel,tklabel(jdl3,text="%"))
tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jdl3,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")
tkgrid(alfa)

dval=tclVar("0")
dent=tkentry(jdl3,width="5",textvariable=dval)
tkgrid(tklabel(jdl3,text="Nilai d0 (d nol):" ))
tkgrid(dent)

#---------------------------------------------------------program import
fy=function()
{ 
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
if(hipo0==" "||hipo1==" ")
  {tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{  
  library(foreign)
  data1=read.spss(file.choose(),use.value.labels=TRUE,max.value.labels=Inf,to.data.frame=TRUE)

periksa=function()
{
require(tcltk)
jendela00<-tktoplevel()
tktitle(jendela00)<-"Input Sampel"
tclRequire("Tktable")

x=c(data1[,1])
y=c(data1[,2])
x=x[x>0]
y=y[y>0]
n1=length(x)
n2=length(y)

kol<-1
isitabel1<- tclArray()
for (i in 0:n1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(n1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")

kol<-1
isitabel2<- tclArray()
for (i in 0:n2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2<-tkwidget(jendela00,"table",variable=isitabel2,rows=(n2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup<-tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}

#-------------------------------------------------------------program uji z
fy1=function()
{    
  a=as.numeric(tclvalue(slidervalue))
  x=c(data1[,1])
  y=c(data1[,2])
  n1=length(x)
  n2=length(y)
  dnol=as.numeric(tclvalue(dval))
  alfa=a/100
  z=(mean(x)-mean(y)-dnol)/sqrt((var(x)/n1)+(var(y)/n2))

#------------------------------------------------------------jendela (hasil)
require(tcltk)
jendela22 <- tktoplevel()
tkwm.title(jendela22, "HASIL ANALISIS UJI Z VARIANSI DIKETAHUI")

hasil2=function()
{
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BEBAS:",font=font1))
tkgrid(tklabel(jendela22,text="ASUMSI 1 : VARIANSI DIKETAHUI (DATA IMPORT)",font=font1))
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HIPOTESIS"))
teks <- tktext(jendela22,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela22,text="\nDATA IMPORT"))
teks2<- tktext(jendela22,height="9")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela22,text="\nDAERAH KRITIS"))
teks3<- tktext(jendela22,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung < -(Z-tabel) atau Z-hitung > Z-tabel") 
  zt=qnorm(1-alfa/2,0,1)
  if(z>-zt && z<zt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung < -(Z-tabel)")
  zt=qnorm(1-alfa,0,1)
  if(z>-zt)a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung > Z-tabel")
  zt=qnorm(1-alfa,0,1)
  if(z<zt) a=0
 }

tkgrid(tklabel(jendela22,text="\nSTATISTIK UJI"))
teks4<- tktext(jendela22,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:\n                    ")
tkinsert(teks4,"end",z)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Statistik tabel:\n                    ")
tkinsert(teks4,"end",zt)

tkgrid(tklabel(jendela22,text="\nHASIL"))
teks5<- tktext(jendela22,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}
tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombols<-tkbutton(jendela22,text="SELESAI",command=function()tkdestroy(jendela22),bg="aquamarine")
tkgrid(tombols)
}
hasil2()
}

tkdestroy(kb)

tombolperiksa<-tkbutton(jdl3,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tbm=tkbutton(jdl3,text="HASIL ANALISIS",command=fy1,bg="gold")
tkgrid(tbm)   

back=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(back,sticky="n",padx=5,pady=5)

}#--------------------------------------------------------------------fy1
}

tkgrid(tklabel(jdl3,text=" "))
mb=tkbutton(jdl3,text="IMPORT",command=fy,bg="skyblue")
tkgrid(mb,sticky="n",padx=5,pady=5)

kb=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(kb,sticky="n",padx=5,pady=5)

tkgrid(tklabel(jdl3,text=" "))
tkgrid(tklabel(jdl3,text="*Catatan : Data Import hanya bisa dengan format *sav (SPSS)"))
tkgrid(tklabel(jdl3,text=" "))
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx





#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx UJI T ASUMSI 2 VAR SAMA DATA RINGKASAN XXXXXXXXXXXXXXXXXXXXXX
asumsi2ringkasan=function()
{
require(tcltk)
jdlr2=tktoplevel()
tktitle(jdlr2)<-"Data Ringkasan Uji T Variansi Sama"

teksaa=tkfont.create(family="sans",weight="bold",size=10)
teksbb=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jdlr2,text="UJI RATA RATA 2 SAMPEL BEBAS",font=teksbb),sticky="n")
tkgrid(tklabel(jdlr2,text="ASUMSI KE 2 (DATA RINGKASAN)",font=teksbb),sticky="n")
tkgrid(tklabel(jdlr2,text=" "))

hip0=tclVar(" ")
h0=tkentry(jdlr2,width="100",textvariable=hip0)
tkgrid(tklabel(jdlr2,text="H0:",font=teksaa))
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jdlr2,width="100",textvariable=hip1)
tkgrid(tklabel(jdlr2,text="H1:",font=teksaa))
tkgrid(h1)

rb1=tkradiobutton(jdlr2)
rb2=tkradiobutton(jdlr2)
rb3=tkradiobutton(jdlr2)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jdlr2,text="Pilihan hipotesis:",font=teksaa))
tkgrid(tklabel(jdlr2,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jdlr2,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jdlr2,text="H1 : Miu1 - Miu2  > d0"),rb3)

tkgrid(tklabel(jdlr2,text=" "))
 nsatu<-tclVar("0")
 eb11=tkentry(jdlr2,width="5",textvariable=nsatu)
 tkgrid(tklabel(jdlr2,text="Banyak Sampel Pertama :"))		 							
 tkgrid(eb11)

 ndua=tclVar("0")
 eb22=tkentry(jdlr2,width="5",textvariable=ndua)
 tkgrid(tklabel(jdlr2,text="Banyak Sampel Kedua:"))
 tkgrid(eb22)

dval=tclVar("0")
dent=tkentry(jdlr2,width="5",textvariable=dval)
tkgrid(tklabel(jdlr2,text="Nilai d0 (d nol) :"))
tkgrid(dent)

slidervalue2=tclVar("5.0")
slidervaluelabel2=tklabel(jdlr2,text=as.numeric(tclvalue(slidervalue2)))
tkgrid(tklabel(jdlr2,text="Nilai alfa :"),slidervaluelabel2,tklabel(jdlr2,text="%"))
tkconfigure(slidervaluelabel2,textvariable=slidervalue2)
alfa=tkscale(jdlr2,from=1,to=20,showvalue=T,variable=slidervalue2,resolution=0.1,orient="horizontal")
tkgrid(alfa)

#----------------------------------------------------------fungsiinputdataringkas2

fungsitabel2=function()
{
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
n1=as.numeric(tclvalue(nsatu))
n2=as.numeric(tclvalue(ndua))

 xb11=tclVar("0")
 xbe1=tkentry(jdlr2,width="5",textvariable=xb11)
 tkgrid(tklabel(jdlr2,text="Rata - Rata Sampel Pertama :"))
 tkgrid(xbe1)

 xb21=tclVar("0")
 xbe2=tkentry(jdlr2,width="5",textvariable=xb21)
 tkgrid(tklabel(jdlr2,text="Rata - Rata Sampel Kedua :"))
 tkgrid(xbe2)

 sd11=tclVar("0")
 sde1=tkentry(jdlr2,width="5",textvariable=sd11)
 tkgrid(tklabel(jdlr2,text="Simpangan Baku Sampel Pertama :"))
 tkgrid(sde1)

 sd21=tclVar("0")
 sde2=tkentry(jdlr2,width="5",textvariable=sd21)
 tkgrid(tklabel(jdlr2,text="Simpangan Baku Sampel Kedua :"))
 tkgrid(sde2)

#----------------------------------------------------------program uji T asumsi 2
inputdr2=function()
{
 a=as.numeric(tclvalue(slidervalue2))
 n1=as.numeric(tclvalue(nsatu))
 n2=as.numeric(tclvalue(ndua))
 xb1=as.numeric(tclvalue(xb11))
 xb2=as.numeric(tclvalue(xb21))
 sd1=as.numeric(tclvalue(sd11))
 sd2=as.numeric(tclvalue(sd21))
 dnol=as.numeric(tclvalue(dval))
 alfa=a/100
 v=n1+n2-2
 b=n1-1
 c=n2-1
 sp2=((b*(sd1^2))+(c*(sd2^2)))/v
 sp=sqrt(sp2)
 t=(xb1-xb2-dnol)/(sp*sqrt((1/n1)+(1/n2)))

#------------------------------------------------------------jendela (hasil)
require(tcltk)
jdlr22 <- tktoplevel()
tkwm.title(jdlr22, "HASIL ANALISIS UJI T VARIANSI SAMA")

hasil2=function()
{
teks11=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jdlr22,text=" "))
tkgrid(tklabel(jdlr22,text="HASIL ANALISIS UJI RATA RATA POULASI 2 SAMPEL BEBAS:",font=teks11))
tkgrid(tklabel(jdlr22,text="ASUMSI 2 : VARIANSI SAMA (DATA RINGKASAN)",font=teks11))
tkgrid(tklabel(jdlr22,text=" "))

tkgrid(tklabel(jdlr22,text="HIPOTESIS"))
teks <- tktext(jdlr22,height="3")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jdlr22,text="\nDATA RINGKASAN"))
teks2<- tktext(jdlr22,height="12")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel ke-1 :                       ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel ke-2 :                       ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Rata - rata sampel ke-1 :                  ")
tkinsert(teks2,"end",xb1)
tkinsert(teks2,"end","\nRata - rata sampel ke-2 :                  ")
tkinsert(teks2,"end",xb2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Simp.Baku sampel ke-1 :                    ")
tkinsert(teks2,"end",sd1)
tkinsert(teks2,"end","\nSimp.Baku sampel ke-2 :                    ")
tkinsert(teks2,"end",sd2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai d nol (d0):                        ")
tkinsert(teks2,"end",dnol)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jdlr22,text="\nDAERAH KRITIS"))
teks3<- tktext(jdlr22,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel) atau T-hitung > T-tabel") 
  tt=qt(1-(alfa/2),v)
  if(t>-tt && t<tt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel)")
  tt=qt(1-alfa,v)
  if(t>-tt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung > T-tabel")
  tt=qt(1-alfa,v)
  if(t<tt) a=0
 } 

tkgrid(tklabel(jdlr22,text="\nSTATISTIK UJI"))
teks4<- tktext(jdlr22,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:                   ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n\n")
tkinsert(teks4,"end","Statistik tabel :                   ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jdlr22,text="\nHASIL"))
teks5<- tktext(jdlr22,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\n\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Terima H1\n\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}
tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tlend<-tkbutton(jdlr22,text="SELESAI",command=function()tkdestroy(jdlr22),bg="aquamarine")
tkgrid(tlend)
}
hasil2()
}

tkdestroy(tblend)

tlprint<-tkbutton(jdlr2,text="HASIL ANALISIS",command=inputdr2,bg="gold")
tkgrid(tlprint,padx=5,pady=5)


balikturn=tkbutton(jdlr2,text="KEMBALI KE MENU",command=function()tkdestroy(jdlr2),bg="turquoise")
tkgrid(balikturn,sticky="n",padx=5,pady=5)

}

tlnext<-tkbutton(jdlr2,text="MASUKKAN DATA",command=fungsitabel2,bg="skyblue")
tkgrid(tlnext,padx=5,pady=5)

tblend<-tkbutton(jdlr2,text="KEMBALI KE MENU",command=function()tkdestroy(jdlr2),bg="turquoise")
tkgrid(tblend,padx=5,pady=5,sticky="n")
}

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx UJI T ASUMSI 2 VAR SAMA DATA INPUT xxxxxxxxxxxxxxxxxxxxxxxxxxxx
asumsi2input=function()
{
#------------------------------jendela input data
require(tcltk)
jendela=tktoplevel()
tktitle(jendela)="Data Input Uji T Asumsi Variansi Sama"

teksa=tkfont.create(family="sans",weight="bold",size=10)
teksb=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jendela,text="UJI RATA RATA 2 SAMPEL BEBAS",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text="ASUMSI KE 2 (INPUT DATA)",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text=" "))

hip0=tclVar(" ")
h0=tkentry(jendela,width="100",textvariable=hip0)
tkgrid(tklabel(jendela,text="H0:",font=teksa))
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jendela,width="100",textvariable=hip1)
tkgrid(tklabel(jendela,text="H1:",font=teksa))
tkgrid(h1)

tkgrid(tklabel(jendela,text=" "))

rb1=tkradiobutton(jendela)
rb2=tkradiobutton(jendela)
rb3=tkradiobutton(jendela)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jendela,text="Pilihan hipotesis:",font=teksa))
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2  > d0"),rb3)

rbVal=as.numeric(tclvalue(rbValue))
tkgrid(tklabel(jendela,text=" "))

n11=tclVar("0")
eb1=tkentry(jendela,width="5",textvariable=n11)
tkgrid(tklabel(jendela,text="Banyak Sampel Pertama :"))
tkgrid(eb1)

n22=tclVar("0")
eb2=tkentry(jendela,width="5",textvariable=n22)
tkgrid(tklabel(jendela,text="Banyak Sampel Kedua :"))
tkgrid(eb2)

dval=tclVar("0")
dent=tkentry(jendela,width="5",textvariable=dval)
tkgrid(tklabel(jendela,text="Nilai d0 (d nol) :"))
tkgrid(dent)

#----------------------------------------------------------fungsitabel1

fungsitabel1=function()
{
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
n1=as.numeric(tclvalue(n11))
n2=as.numeric(tclvalue(n22))

#-----------------------------------------------------------------if
  	if(hipo0==" "||hipo1==" ")
    	{tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{
	if(n1==0||n2==0)
	{tkmessageBox(message="ANDA BELUM MENGINPUT BANYAK DATA DENGAN BENAR",icon="warning")}
else
{
#-------------------------------------------------------------jendela1
require(tcltk)
jendela1=tktoplevel()
tktitle(jendela1)<-"Input Sampel"

teks1=tkfont.create(family="sans",weight="bold",size=14)
	tkgrid(tklabel(jendela1,text="MASUKKAN DATA",font=teks1))
	tkgrid(tklabel(jendela1,text=" "))
	tkgrid(tklabel(jendela1,text=" "))
tkgrid(tklabel(jendela1,text="Sampel pertama :                   Sampel kedua :",font=teks1),sticky="n")

#----------------------------------------------------------------input tabel1
tclRequire("Tktable")
bar1=as.numeric(tclvalue(n11))
kol=1
wadah1=matrix(0,bar1,kol)
x<-matrix(0,bar1,kol)
isitabel1= tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-wadah1[i,j]
 }
}
isitabel1[[0,0]]="n1"

tabel1=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel1=tkwidget(jendela1,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


#---------------------------------------------------------input tabel2
bar2=as.numeric(tclvalue(n22))
kol=1
wadah2=matrix(0,bar2,kol)
y<-matrix(0,bar2,kol)
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-wadah2[i,j]
 }
}
isitabel2[[0,0]]="n2"

tabel2=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel2=tkwidget(jendela1,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jendela,text=as.numeric(tclvalue(slidervalue)))

tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jendela,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")


periksa=function()
{
require(tcltk)
jendela00=tktoplevel()
tktitle(jendela00)="DATA INPUTAN"
tclRequire("Tktable")
for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

bar1=as.numeric(tclvalue(n11))
kol=1
isitabel1=tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"
tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


bar2=as.numeric(tclvalue(n22))
kol=1
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2=tkwidget(jendela00,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup=tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")
}

input1=function()    #print nilai fungsi n1
{
 for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)


#--------------------------------------------------------program uji t
n1=length(x)
n2=length(y)
a=as.numeric(tclvalue(slidervalue))
alfa=a/100
dnol=as.numeric(tclvalue(dval))
v=n1+n2-2
sp=((n1-1)*var(x)+(n2-1)*var(y))/v
t=(mean(x)-mean(y)-dnol)/(sqrt(sp)*sqrt((1/n1)+(1/n2)))

#----------------------------------------------------------jendela2(hasil)
require(tcltk)
jendela2=tktoplevel()
tkwm.title(jendela2, "HASIL ANALISIS UJI T VARIANSI SAMA")

hasil=function()
{
tkgrid(tklabel(jendela2,text=" "))
tkgrid(tklabel(jendela2,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BEBAS:",font=teks1))
tkgrid(tklabel(jendela2,text="ASUMSI 2 : VARIANSI SAMA (DATA INPUT)",font=teks1))
tkgrid(tklabel(jendela2,text=" "))


tkgrid(tklabel(jendela2,text="HIPOTESIS"))
teks=tktext(jendela2,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela2,text="\nDATA INPUTAN"))
teks2=tktext(jendela2,height="7")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela2,text="\nDAERAH KRITIS"))
teks3=tktext(jendela2,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)  
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel) atau T-hitung > T-tabel") 
  tt=qt(1-alfa/2,v)
  if(t>-tt && t<tt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel)")
  tt=qt(1-alfa,v)
  if(t>-tt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung > T-tabel")
  tt=qt(1-alfa,v)
  if(t<tt) a=0
 } 


tkgrid(tklabel(jendela2,text="\nSTATISTIK UJI"))
teks4=tktext(jendela2,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:\n                    ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Statistik tabel:\n                    ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jendela2,text="\nHASIL"))
teks5<- tktext(jendela2,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}

tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombolse<-tkbutton(jendela2,text="SELESAI",command=function()tkdestroy(jendela2),bg="aquamarine")
tkgrid(tombolse,sticky="n")
tkgrid(tklabel(jendela2,text=" "))
}
hasil()
}

tkdestroy(tmblselesai)

tombolperiksa<-tkbutton(jendela,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tkgrid(tklabel(jendela,text="Nilai alfa:"),slidervaluelabel,tklabel(jendela,text="%"))
tkgrid(alfa)

tombolprint<-tkbutton(jendela,text="HASIL ANALISIS",command=input1,bg="gold")
tkgrid(tombolprint,padx=5,pady=5)

selesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(selesai,padx=5,pady=5,sticky="n")

tombolselesai1<-tkbutton(jendela1,text="SELESAI",command=function()tkdestroy(jendela1),bg="aquamarine")
tkgrid(tombolselesai1,sticky="n")
}
}
}

tkgrid(tklabel(jendela,text=" "))
tombolnext<-tkbutton(jendela,text="MASUKKAN DATA",command=fungsitabel1,bg="skyblue")
tkgrid(tombolnext,padx=5,pady=5)

tmblselesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(tmblselesai,padx=5,pady=5,sticky="n")
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx UJI T ASUMSI VAR SAMA DATA IMPORT XXXXXXXXXXXXXXXX
asumsi2import=function()
{
require(tcltk)
jdl3=tktoplevel()
tktitle(jdl3)="Data Import SPSS Uji T Asumsi Variansi Sama"
  

font1=tkfont.create(family="sans",size=14,weight="bold")
font2=tkfont.create(family="times",size=10,weight="bold")
tkgrid(tklabel(jdl3,text="UJI 2 RATA RATA SAMPEL BEBAS",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text="ASUMSI KE 2 (DATA IMPORT)",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text=" "))


hip0=tclVar(" ")
h0=tkentry(jdl3,width="100",textvariable=hip0) 
tkgrid(tklabel(jdl3,text="H0:",font=font2)) 
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jdl3,width="100",textvariable=hip1)
tkgrid(tklabel(jdl3,text="H1:",font=font2))
tkgrid(h1) 
teks3=tclVar()

tkgrid(tklabel(jdl3,text=" "))

rb1=tkradiobutton(jdl3)
rb2=tkradiobutton(jdl3)
rb3=tkradiobutton(jdl3)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jdl3,text="Pilihan hipotesis:",font=font2))
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2  > d0"),rb3)
                
tkgrid(tklabel(jdl3,text=" "))
slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jdl3,text=as.numeric(tclvalue(slidervalue)))
tkgrid(tklabel(jdl3,text="Nilai alfa:"),slidervaluelabel,tklabel(jdl3,text="%"))
tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jdl3,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")
tkgrid(alfa)

dval=tclVar("0")
dent=tkentry(jdl3,width="5",textvariable=dval)
tkgrid(tklabel(jdl3,text="Nilai d0 (d nol) :"))
tkgrid(dent)
 

fy=function()
{ 
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
if(hipo0==" "||hipo1==" ")
  {tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{  
  library(foreign)
  data1=read.spss(file.choose(),use.value.labels=TRUE,max.value.labels=Inf,to.data.frame=TRUE)

periksa=function()
{
require(tcltk)
jendela00<-tktoplevel()
tktitle(jendela00)<-"input sampel"
tclRequire("Tktable")

x=c(data1[,1])
y=c(data1[,2])
x=x[x>0]
y=y[y>0]
n1=length(x)
n2=length(y)

kol<-1
isitabel1<- tclArray()
for (i in 0:n1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(n1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


kol<-1
isitabel2<- tclArray()
for (i in 0:n2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2<-tkwidget(jendela00,"table",variable=isitabel2,rows=(n2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup<-tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}


fy1=function()
{    
  a=as.numeric(tclvalue(slidervalue))
  x=c(data1[,1])
  y=c(data1[,2])
  n1=length(x)
  n2=length(y)
  dnol=as.numeric(tclvalue(dval))
  alfa=a/100
  v=n1+n2-2
  sp=((n1-1)*var(x)+(n2-1)*var(y))/v
  t=(mean(x)-mean(y)-dnol)/(sqrt(sp)*sqrt((1/n1)+(1/n2)))


require(tcltk)
jendela22 <- tktoplevel()
tkwm.title(jendela22, "HASIL ANALISIS UJI T VARIANSI SAMA")

hasil2=function()
{
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BEBAS:",font=font1))
tkgrid(tklabel(jendela22,text="ASUMSI 2 : VARIANSI SAMA (DATA IMPORT)",font=font1))
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HIPOTESIS"))
teks <- tktext(jendela22,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela22,text="\nDATA IMPORT"))
teks2<- tktext(jendela22,height="7")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela22,text="\nDAERAH KRITIS"))
teks3<- tktext(jendela22,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel) atau T-hitung > T-tabel") 
  tt=qt(1-alfa/2,v)
  if(t>-tt && t<tt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel)")
  tt=qt(1-alfa,v)
  if(t>-tt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung > T-tabel")
  tt=qt(1-alfa,v)
  if(t<tt) a=0
 }

tkgrid(tklabel(jendela22,text="\nSTATISTIK UJI"))
teks4<- tktext(jendela22,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:\n                    ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Statistik tabel:\n                    ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jendela22,text="\nHASIL"))
teks5<- tktext(jendela22,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if(a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}
tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombols<-tkbutton(jendela22,text="SELESAI",command=function()tkdestroy(jendela22),bg="aquamarine")
tkgrid(tombols)
}
hasil2()
}

tkdestroy(kb)

tombolperiksa<-tkbutton(jdl3,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tbm=tkbutton(jdl3,text="HASIL ANALISIS",command=fy1,bg="gold")
tkgrid(tbm)   

back=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(back,sticky="n",padx=5,pady=5)
} 
}

tkgrid(tklabel(jdl3,text=" "))
mb=tkbutton(jdl3,text="IMPORT",command=fy,bg="skyblue")
tkgrid(mb,sticky="n",padx=5,pady=5)

kb=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(kb,sticky="n",padx=5,pady=5)

tkgrid(tklabel(jdl3,text=" "))
 tkgrid(tklabel(jdl3,text="*Catatan : Data Import hanya bisa dengan format *sav (SPSS)"))
tkgrid(tklabel(jdl3,text=" "))
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx







#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx UJI T ASUMSI VAR TIDAK SAMA DATA RINGKASAN XXXXXXXXXXXXXXXX
asumsi3ringkasan=function()
{
require(tcltk)
jdlr3=tktoplevel()
tktitle(jdlr3)<-"Data Ringkasan Uji T Asumsi Variansi Tidak Sama"

teksaa=tkfont.create(family="sans",weight="bold",size=10)
teksbb=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jdlr3,text="UJI RATA RATA 2 SAMPEL BEBAS",font=teksbb),sticky="n")
tkgrid(tklabel(jdlr3,text="ASUMSI KE 3 (DATA RINGKASAN)",font=teksbb),sticky="n")
tkgrid(tklabel(jdlr3,text=" "))

hip0=tclVar(" ")
h0=tkentry(jdlr3,width="100",textvariable=hip0)
tkgrid(tklabel(jdlr3,text="H0:",font=teksaa))
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jdlr3,width="100",textvariable=hip1)
tkgrid(tklabel(jdlr3,text="H1:",font=teksaa))
tkgrid(h1)

rb1=tkradiobutton(jdlr3)
rb2=tkradiobutton(jdlr3)
rb3=tkradiobutton(jdlr3)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jdlr3,text="Pilihan hipotesis:",font=teksaa))
tkgrid(tklabel(jdlr3,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jdlr3,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jdlr3,text="H1 : Miu1 - Miu2  > d0"),rb3)

tkgrid(tklabel(jdlr3,text=" "))
 nsatu<-tclVar("0")
 eb11=tkentry(jdlr3,width="5",textvariable=nsatu)
 tkgrid(tklabel(jdlr3,text="Banyak Sampel Pertama :"))		 							
 tkgrid(eb11)

 ndua=tclVar("0")
 eb22=tkentry(jdlr3,width="5",textvariable=ndua)
 tkgrid(tklabel(jdlr3,text="Banyak Sampel Kedua:"))
 tkgrid(eb22)

dval=tclVar("0")
dent=tkentry(jdlr3,width="5",textvariable=dval)
tkgrid(tklabel(jdlr3,text="Nilai d0 (d nol) :"))
tkgrid(dent)

slidervalue1=tclVar("5.0")
slidervaluelabel1=tklabel(jdlr3,text=as.numeric(tclvalue(slidervalue1)))
tkgrid(tklabel(jdlr3,text="Nilai alfa :"),slidervaluelabel1,tklabel(jdlr3,text="%"))
tkconfigure(slidervaluelabel1,textvariable=slidervalue1)
alfa=tkscale(jdlr3,from=1,to=20,showvalue=T,variable=slidervalue1,resolution=0.1,orient="horizontal")
tkgrid(alfa)

#----------------------------------------------------------fungsiinputdataringkas3
fungsitabel3=function()
{
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
n1=as.numeric(tclvalue(nsatu))
n2=as.numeric(tclvalue(ndua))

 xb11=tclVar("0")
 xbe1=tkentry(jdlr3,width="5",textvariable=xb11)
 tkgrid(tklabel(jdlr3,text="Rata - Rata Sampel Pertama :"))
 tkgrid(xbe1)

 xb21=tclVar("0")
 xbe2=tkentry(jdlr3,width="5",textvariable=xb21)
 tkgrid(tklabel(jdlr3,text="Rata - Rata Sampel Kedua :"))
 tkgrid(xbe2)

 sd11=tclVar("0")
 sde1=tkentry(jdlr3,width="5",textvariable=sd11)
 tkgrid(tklabel(jdlr3,text="Simpangan Baku Sampel Pertama :"))
 tkgrid(sde1)

 sd21=tclVar("0")
 sde2=tkentry(jdlr3,width="5",textvariable=sd21)
 tkgrid(tklabel(jdlr3,text="Simpangan Baku Sampel Kedua :"))
 tkgrid(sde2)

#----------------------------------------------------------program uji T
inputdr3=function()
{
 a=as.numeric(tclvalue(slidervalue1))
 n1=as.numeric(tclvalue(nsatu))
 n2=as.numeric(tclvalue(ndua))
 xb1=as.numeric(tclvalue(xb11))
 xb2=as.numeric(tclvalue(xb21))
 sd1=as.numeric(tclvalue(sd11))
 sd2=as.numeric(tclvalue(sd21))
 dnol=as.numeric(tclvalue(dval))
 alfa=a/100
 v=(((sd1^2/n1)+(sd2^2/n2))^2)/((((sd1^2/n1)^2)/(n1-1))+(((sd2^2/n2)^2)/(n2-1)))
 t=(xb1-xb2-dnol)/sqrt((sd1^2/n1)+(sd2^2/n2))

#------------------------------------------------------------jendela (hasil)
require(tcltk)
jdlr33 <- tktoplevel()
tkwm.title(jdlr33, "HASIL ANALISIS UJI T VARIANSI TIDAK SAMA")

hasil31=function()
{
teks11=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jdlr33,text=" "))
tkgrid(tklabel(jdlr33,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BEBAS:",font=teks11))
tkgrid(tklabel(jdlr33,text="ASUMSI 3 : VARIANSI TIDAK SAMA (DATA RINGKASAN)",font=teks11))
tkgrid(tklabel(jdlr33,text=" "))

tkgrid(tklabel(jdlr33,text="HIPOTESIS"))
teks <- tktext(jdlr33,height="3")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jdlr33,text="\nDATA RINGKASAN"))
teks2<- tktext(jdlr33,height="12")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel ke-1 :                       ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel ke-2 :                       ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Rata - rata sampel ke-1 :                  ")
tkinsert(teks2,"end",xb1)
tkinsert(teks2,"end","\nRata - rata sampel ke-2 :                  ")
tkinsert(teks2,"end",xb2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Simp.Baku sampel ke-1 :                    ")
tkinsert(teks2,"end",sd1)
tkinsert(teks2,"end","\nSimp.Baku sampel ke-2 :                    ")
tkinsert(teks2,"end",sd2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai d nol (d0):                        ")
tkinsert(teks2,"end",dnol)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jdlr33,text="\nDAERAH KRITIS"))
teks3<- tktext(jdlr33,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel) atau T-hitung > T-tabel") 
  tt=qt(1-alfa,v)
  if(t>-tt || t<tt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel)")
    tt=qt(1-alfa,v)
  if(t>-tt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung > T-tabel")
    tt=qt(1-alfa,v)
  if(t<tt) a=0
 }

tkgrid(tklabel(jdlr33,text="\nSTATISTIK UJI"))
teks4<- tktext(jdlr33,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:                   ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n\n")
tkinsert(teks4,"end","Statistik tabel :                   ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jdlr33,text="\nHASIL"))
teks5<- tktext(jdlr33,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\n\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Terima H1\n\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}
tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tblend<-tkbutton(jdlr33,text="SELESAI",command=function()tkdestroy(jdlr33),bg="aquamarine")
tkgrid(tblend)
}
hasil31()
}
tkdestroy(tmbolend)

tomblprint<-tkbutton(jdlr3,text="HASIL ANALISIS",command=inputdr3,bg="gold")
tkgrid(tomblprint,padx=5,pady=5)


balikkmbl=tkbutton(jdlr3,text="KEMBALI KE MENU",command=function()tkdestroy(jdlr3),bg="turquoise")
tkgrid(balikkmbl,sticky="n",padx=5,pady=5)

}#--------------------------------------------------------------------inputdr

tomblnext<-tkbutton(jdlr3,text="MASUKKAN DATA",command=fungsitabel3,bg="skyblue")
tkgrid(tomblnext,padx=5,pady=5)

tmbolend<-tkbutton(jdlr3,text="KEMBALI KE MENU",command=function()tkdestroy(jdlr3),bg="turquoise")
tkgrid(tmbolend,padx=5,pady=5,sticky="n")
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx  UJI T ASUMSI VAR TIDAK SAMA DATA INPUT  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
asumsi3input=function()
{
#------------------------------jendela input data
require(tcltk)
jendela=tktoplevel()
tktitle(jendela)<-"Data Input Uji T Asumsi Variansi Tidak Sama"

teksa=tkfont.create(family="sans",weight="bold",size=10)
teksb=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jendela,text="UJI RATA RATA 2 SAMPEL BEBAS",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text="ASUMSI KE 3 (DATA INPUT)",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text=" "))

hip0=tclVar(" ")
h0=tkentry(jendela,width="100",textvariable=hip0)
tkgrid(tklabel(jendela,text="H0:",font=teksa))
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jendela,width="100",textvariable=hip1)
tkgrid(tklabel(jendela,text="H1:",font=teksa))
tkgrid(h1)

tkgrid(tklabel(jendela,text=" "))

rb1=tkradiobutton(jendela)
rb2=tkradiobutton(jendela)
rb3=tkradiobutton(jendela)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jendela,text="Pilihan hipotesis:",font=teksa))
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2  > d0"),rb3)

rbVal=as.numeric(tclvalue(rbValue))

tkgrid(tklabel(jendela,text=" "))

n11=tclVar("0")
eb1=tkentry(jendela,width="5",textvariable=n11)
tkgrid(tklabel(jendela,text="Banyak Sampel Pertama :"))
tkgrid(eb1)

n22=tclVar("0")
eb2=tkentry(jendela,width="5",textvariable=n22)
tkgrid(tklabel(jendela,text="Banyak Sampel Kedua :"))
tkgrid(eb2)

dval=tclVar("0")
dent=tkentry(jendela,width="5",textvariable=dval)
tkgrid(tklabel(jendela,text="Nilai d0 (d nol) :"))
tkgrid(dent)

#----------------------------------------------------------fungsitabel1

fungsitabel1=function()
{
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
n1=as.numeric(tclvalue(n11))
n2=as.numeric(tclvalue(n22))

#-----------------------------------------------------------------if
  	if(hipo0==" "||hipo1==" ")
    	{tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{
	if(n1==0||n2==0)
	{tkmessageBox(message="ANDA BELUM MENGINPUT BANYAK DATA DENGAN BENAR",icon="warning")}
else
{
#-------------------------------------------------------------jendela1
require(tcltk)
jendela1=tktoplevel()
tktitle(jendela1)<-"Input Sampel"


teks1=tkfont.create(family="sans",weight="bold",size=14)
	tkgrid(tklabel(jendela1,text="MASUKKAN DATA",font=teks1))
	tkgrid(tklabel(jendela1,text=" "))
	tkgrid(tklabel(jendela1,text=" "))
tkgrid(tklabel(jendela1,text="Sampel pertama :                   Sampel kedua :",font=teks1),sticky="n")

#----------------------------------------------------------------input tabel1
tclRequire("Tktable")
bar1=as.numeric(tclvalue(n11))
kol=1
wadah1=matrix(0,bar1,kol)
x<-matrix(0,bar1,kol)
isitabel1= tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-wadah1[i,j]
 }
}
isitabel1[[0,0]]="n1"

tabel1=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel1=tkwidget(jendela1,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")

#---------------------------------------------------------input tabel2
bar2=as.numeric(tclvalue(n22))
kol=1
wadah2=matrix(0,bar2,kol)
y<-matrix(0,bar2,kol)
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-wadah2[i,j]
 }
}
isitabel2[[0,0]]="n2"

tabel2=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel2=tkwidget(jendela1,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jendela,text=as.numeric(tclvalue(slidervalue)))

tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jendela,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")


periksa=function()
{
require(tcltk)
jendela00=tktoplevel()
tktitle(jendela00)="Input Sampel"
tclRequire("Tktable")

for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

bar1=as.numeric(tclvalue(n11))
kol=1
isitabel1=tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


bar2=as.numeric(tclvalue(n22))
kol=1
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2=tkwidget(jendela00,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup=tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}


input1=function()    #print nilai fungsi n1
{
 for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

#--------------------------------------------------------program uji t
n1=length(x)
n2=length(y)
a=as.numeric(tclvalue(slidervalue))
alfa=a/100
dnol=as.numeric(tclvalue(dval))
s1=(sd(x)^2)/n1
s2=(sd(y)^2)/n2
v=(s1+s2)^2/((s1^2/(n1-1))+(s2^2/(n2-1)))
t=(mean(x)-mean(y)-dnol)/sqrt(s1+s2)

#----------------------------------------------------------jendela2(hasil)
require(tcltk)
jendela2=tktoplevel()
tkwm.title(jendela2, "HASIL ANALISIS UJI T VARIANSI TIDAK SAMA")

hasil=function()
{
tkgrid(tklabel(jendela2,text=" "))
tkgrid(tklabel(jendela2,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BEBAS:",font=teks1))
tkgrid(tklabel(jendela2,text="ASUMSI 3 : VARIANSI TIDAK SAMA(DATA INPUT)",font=teks1))
tkgrid(tklabel(jendela2,text=" "))
tkgrid(tklabel(jendela2,text="HIPOTESIS"))
teks=tktext(jendela2,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela2,text="\nDATA INPUTAN"))
teks2=tktext(jendela2,height="7")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela2,text="\nDAERAH KRITIS"))
teks3=tktext(jendela2,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3) 
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel) atau T-hitung > T-tabel") 
  tt=qt(1-alfa/2,v)
  if(t>-tt && t<tt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel)")
  tt=qt(1-alfa,v)
  if(t>-tt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung > T-tabel")
  tt=qt(1-alfa,v)
  if(t<tt) a=0
 } 

tkgrid(tklabel(jendela2,text="\nSTATISTIK UJI"))
teks4=tktext(jendela2,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:\n                    ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Statistik tabel:\n                    ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jendela2,text="\nHASIL"))
teks5<- tktext(jendela2,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}

tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombolse<-tkbutton(jendela2,text="SELESAI",command=function()tkdestroy(jendela2),bg="aquamarine")
tkgrid(tombolse,sticky="n")
tkgrid(tklabel(jendela2,text=" "))

}
hasil()
}


tkdestroy(tmblselesai)

tombolperiksa<-tkbutton(jendela,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tkgrid(tklabel(jendela,text="Nilai alfa:"),slidervaluelabel,tklabel(jendela,text="%"))
tkgrid(alfa)

tombolprint<-tkbutton(jendela,text="HASIL ANALISIS",command=input1,bg="gold")
tkgrid(tombolprint,padx=5,pady=5)

selesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(selesai,padx=5,pady=5,sticky="n")

tombolselesai1<-tkbutton(jendela1,text="SELESAI",command=function()tkdestroy(jendela1),bg="aquamarine")
tkgrid(tombolselesai1,sticky="n")
}
} #-----------------------------------------------------tutup if
}


tkgrid(tklabel(jendela,text=" "))
tombolnext<-tkbutton(jendela,text="MASUKKAN DATA",command=fungsitabel1,bg="skyblue")
tkgrid(tombolnext,padx=5,pady=5)

tmblselesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(tmblselesai,padx=5,pady=5,sticky="n")
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  UJI T VARIAN TIDAK SAMA DATA IMPORT  xxxxxxxxxxxxxxxxxxxxxxxxxxxx
asumsi3import=function()
{

require(tcltk)
jdl3=tktoplevel()
tktitle(jdl3)="Data Import SPSS Uji T Asumsi Variansi Tidak Sama"
  

font1=tkfont.create(family="sans",size=14,weight="bold")
font2=tkfont.create(family="times",size=10,weight="bold")
tkgrid(tklabel(jdl3,text="UJI 2 RATA RATA SAMPEL BEBAS",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text="ASUMSI KE 3 (DATA IMPORT)",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text=" "))


hip0=tclVar(" ")
h0=tkentry(jdl3,width="100",textvariable=hip0) 
tkgrid(tklabel(jdl3,text="H0:",font=font2)) 
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jdl3,width="100",textvariable=hip1)
tkgrid(tklabel(jdl3,text="H1:",font=font2))
tkgrid(h1) 
teks3=tclVar()

tkgrid(tklabel(jdl3,text=" "))

rb1=tkradiobutton(jdl3)
rb2=tkradiobutton(jdl3)
rb3=tkradiobutton(jdl3)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jdl3,text="Pilihan hipotesis:",font=font2))
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2  > d0"),rb3)
                
tkgrid(tklabel(jdl3,text=" "))
slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jdl3,text=as.numeric(tclvalue(slidervalue)))
tkgrid(tklabel(jdl3,text="Nilai alfa:"),slidervaluelabel,tklabel(jdl3,text="%"))
tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jdl3,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")
tkgrid(alfa)

dval=tclVar("0")
dent=tkentry(jdl3,width="5",textvariable=dval)
tkgrid(tklabel(jdl3,text="Nilai d0 (d nol) :"))
tkgrid(dent)
 
#---------------------------------------------------------program import
fy=function()
{ 
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
if(hipo0==" "||hipo1==" ")
  {tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{  
  library(foreign)
  data1=read.spss(file.choose(),use.value.labels=TRUE,max.value.labels=Inf,to.data.frame=TRUE)


periksa=function()
{
require(tcltk)
jendela00<-tktoplevel()
tktitle(jendela00)<-"Input Sampel"
tclRequire("Tktable")

x=c(data1[,1])
y=c(data1[,2])
x=x[x>0]
y=y[y>0]
n1=length(x)
n2=length(y)

kol<-1
isitabel1<- tclArray()
for (i in 0:n1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(n1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


kol<-1
isitabel2<- tclArray()
for (i in 0:n2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2<-tkwidget(jendela00,"table",variable=isitabel2,rows=(n2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup<-tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}

#-------------------------------------------------------------program uji t
fy1=function()
{    
  a=as.numeric(tclvalue(slidervalue))
  x=c(data1[,1])
  y=c(data1[,2])
  n1=length(x)
  n2=length(y)
  dnol=as.numeric(tclvalue(dval))
  alfa=a/100
  s1=(sd(x)^2)/n1
  s2=(sd(y)^2)/n2
  v=(s1+s2)^2/((s1^2/(n1-1))+(s2^2/(n2-1)))
  t=(mean(x)-mean(y)-dnol)/sqrt(s1+s2)

#------------------------------------------------------------jendela (hasil)
require(tcltk)
jendela22 <- tktoplevel()
tkwm.title(jendela22, "HASIL ANALISIS UJI T VARIANSI TIDAK SAMA")

hasil2=function()
{
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BEBAS:",font=font1))
tkgrid(tklabel(jendela22,text="ASUMSI 3 : VARIANSI TIDAK SAMA (DATA IMPORT)",font=font1))
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HIPOTESIS"))
teks <- tktext(jendela22,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela22,text="\nDATA IMPORT"))
teks2<- tktext(jendela22,height="7")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela22,text="\nDAERAH KRITIS"))
teks3<- tktext(jendela22,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel) atau T-hitung > T-tabel") 
  tt=qt(1-alfa/2,v)
  if(t>-tt && t<tt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel)")
  tt=qt(1-alfa,v)
  if(t>-tt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung > T-tabel")
  tt=qt(1-alfa,v)
  if(t<tt) a=0
 }

tkgrid(tklabel(jendela22,text="\nSTATISTIK UJI"))
teks4<- tktext(jendela22,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:\n                    ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Statistik tabel:\n                    ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jendela22,text="\nHASIL"))
teks5<- tktext(jendela22,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if(a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}
tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombols<-tkbutton(jendela22,text="SELESAI",command=function()tkdestroy(jendela22),bg="aquamarine")
tkgrid(tombols)
}
hasil2()
}

tkdestroy(kb)

tombolperiksa<-tkbutton(jdl3,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tbm=tkbutton(jdl3,text="HASIL ANALISIS",command=fy1,bg="gold")
tkgrid(tbm)   

back=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(back,sticky="n",padx=5,pady=5)
}#-------------------------------------------------------------------- tutup if 
}

tkgrid(tklabel(jdl3,text=" "))
mb=tkbutton(jdl3,text="IMPORT",command=fy,bg="skyblue")
tkgrid(mb,sticky="n",padx=5,pady=5)

kb=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(kb,sticky="n",padx=5,pady=5)

tkgrid(tklabel(jdl3,text=" "))
 tkgrid(tklabel(jdl3,text="*Catatan : Data Import hanya bisa dengan format *sav (SPSS)"))
tkgrid(tklabel(jdl3,text=" "))
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx UJI PAIRED DATA RINGKASAN XXXXXXXXXXXXXXXXXXXXXX
asumsi4ringkasan=function()
{
require(tcltk)
jdlr4=tktoplevel()
tktitle(jdlr4)<-"Data Ringkasan Paired Test"

teksaa=tkfont.create(family="sans",weight="bold",size=10)
teksbb=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jdlr4,text="UJI RATA RATA 2 SAMPEL BERPASANGAN",font=teksbb),sticky="n")
tkgrid(tklabel(jdlr4,text="PAIRED TEST (DATA RINGKASAN)",font=teksbb),sticky="n")
tkgrid(tklabel(jdlr4,text=" "))

hip0=tclVar(" ")
h0=tkentry(jdlr4,width="100",textvariable=hip0)
tkgrid(tklabel(jdlr4,text="H0:",font=teksaa))
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jdlr4,width="100",textvariable=hip1)
tkgrid(tklabel(jdlr4,text="H1:",font=teksaa))
tkgrid(h1)

rb1=tkradiobutton(jdlr4)
rb2=tkradiobutton(jdlr4)
rb3=tkradiobutton(jdlr4)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jdlr4,text="Pilihan hipotesis:",font=teksaa))
tkgrid(tklabel(jdlr4,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jdlr4,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jdlr4,text="H1 : Miu1 - Miu2  > d0"),rb3)

tkgrid(tklabel(jdlr4,text=" "))
 nsatu<-tclVar("0")
 eb11=tkentry(jdlr4,width="5",textvariable=nsatu)
 tkgrid(tklabel(jdlr4,text="Banyak Sampel Berpasangan :"))		 							
 tkgrid(eb11)

dval=tclVar("0")
dent=tkentry(jdlr4,width="5",textvariable=dval)
tkgrid(tklabel(jdlr4,text="Nilai d0 (d nol) :"))
tkgrid(dent)

slidervalue4=tclVar("5.0")
slidervaluelabel4=tklabel(jdlr4,text=as.numeric(tclvalue(slidervalue4)))
tkgrid(tklabel(jdlr4,text="Nilai alfa :"),slidervaluelabel4,tklabel(jdlr4,text="%"))
tkconfigure(slidervaluelabel4,textvariable=slidervalue4)
alfa=tkscale(jdlr4,from=1,to=20,showvalue=T,variable=slidervalue4,resolution=0.1,orient="horizontal")
tkgrid(alfa)

#----------------------------------------------------------fungsiinputdataringkas1
fungsitabel4=function()
{
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
n1=as.numeric(tclvalue(nsatu))

 xb11=tclVar("0")
 xbe1=tkentry(jdlr4,width="5",textvariable=xb11)
 tkgrid(tklabel(jdlr4,text="Rata - Rata Sampel Berpasangan (d bar) :"))
 tkgrid(xbe1)

 sd11=tclVar("0")
 sde1=tkentry(jdlr4,width="5",textvariable=sd11)
 tkgrid(tklabel(jdlr4,text="Simpangan Baku Sampel Berpasangan (Sd) :"))
 tkgrid(sde1)

 
#----------------------------------------------------------program uji paired Test
inputdr4=function()
{
 a=as.numeric(tclvalue(slidervalue4))
 n=as.numeric(tclvalue(nsatu))
 xb=as.numeric(tclvalue(xb11))
 sd=as.numeric(tclvalue(sd11))
 dnol=as.numeric(tclvalue(dval))
 alfa=a/100
 v=n-1
 t=(xb-dnol)/(sd/sqrt(n))

#------------------------------------------------------------jendela (hasil)
require(tcltk)
jdlr44 <- tktoplevel()
tkwm.title(jdlr44, "HASIL ANALISIS PAIRED TEST")

hasil4=function()
{
teks11=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jdlr44,text=" "))
tkgrid(tklabel(jdlr44,text="HASIL ANALISIS UJI RATA-RATA POPULASI 2 SAMPEL BERPASANGAN:",font=teks11))
tkgrid(tklabel(jdlr44,text="PAIRED TEST (DATA RINGKASAN)",font=teks11))
tkgrid(tklabel(jdlr44,text=" "))

tkgrid(tklabel(jdlr44,text="HIPOTESIS"))
teks <- tktext(jdlr44,height="3")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jdlr44,text="\nDATA RINGKASAN"))
teks2<- tktext(jdlr44,height="11")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel berpasangan (N) :                  ")
tkinsert(teks2,"end",n)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Rata - rata sampel berpasangan:                  ")
tkinsert(teks2,"end",xb)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Simp.Baku sampel berpasangan  :                  ")
tkinsert(teks2,"end",sd)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai d nol (d0):                              ")
tkinsert(teks2,"end",dnol)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jdlr44,text="\nDAERAH KRITIS"))
teks3<- tktext(jdlr44,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung < -(Z-tabel) atau Z-hitung > Z-tabel") 
  tt=qt(1-alfa/2,v)
  if(t>-tt || z<zt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung < -(Z-tabel)")
  tt=qt(1-alfa,v)
  if(t>-tt)a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai Z-hitung > Z-tabel")
  tt=qt(1-alfa,v)
  if(t<tt) a=0
 }

tkgrid(tklabel(jdlr44,text="\nSTATISTIK UJI"))
teks4<- tktext(jdlr44,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:                   ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n\n")
tkinsert(teks4,"end","Statistik tabel :                   ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jdlr44,text="\nHASIL"))
teks5<- tktext(jdlr44,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\n\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Terima H1\n\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}
tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tend<-tkbutton(jdlr44,text="SELESAI",command=function()tkdestroy(jdlr44),bg="aquamarine")
tkgrid(tend)
}
hasil4()
}

tkdestroy(tmlend)

tmlprint<-tkbutton(jdlr4,text="HASIL ANALISIS",command=inputdr4,bg="gold")
tkgrid(tmlprint,padx=5,pady=5)


back=tkbutton(jdlr4,text="KEMBALI KE MENU",command=function()tkdestroy(jdlr4),bg="turquoise")
tkgrid(back,sticky="n",padx=5,pady=5)

}#--------------------------------------------------------------------inputdr

tmlnext<-tkbutton(jdlr4,text="MASUKKAN DATA",command=fungsitabel4,bg="skyblue")
tkgrid(tmlnext,padx=5,pady=5)

tmlend<-tkbutton(jdlr4,text="KEMBALI KE MENU",command=function()tkdestroy(jdlr4),bg="turquoise")
tkgrid(tmlend,padx=5,pady=5,sticky="n")
}

#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx  UJI PAIRED DATA INPUT  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
asumsi4input=function()
{
#------------------------------jendela input data
require(tcltk)
jendela=tktoplevel()
tktitle(jendela)<-"Data Input Paired Test"

teksa=tkfont.create(family="sans",weight="bold",size=10)
teksb=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jendela,text="UJI RATA RATA 2 SAMPEL BERPASANGAN",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text="PAIRED TEST (DATA INPUT)",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text=" "))

hip0=tclVar(" ")
h0=tkentry(jendela,width="100",textvariable=hip0)
tkgrid(tklabel(jendela,text="H0:",font=teksa))
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jendela,width="100",textvariable=hip1)
tkgrid(tklabel(jendela,text="H1:",font=teksa))
tkgrid(h1)

tkgrid(tklabel(jendela,text=" "))

rb1=tkradiobutton(jendela)
rb2=tkradiobutton(jendela)
rb3=tkradiobutton(jendela)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jendela,text="Pilihan hipotesis:",font=teksa))
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jendela,text="H1 : Miu1 - Miu2  > d0"),rb3)

rbVal=as.numeric(tclvalue(rbValue))

tkgrid(tklabel(jendela,text=" "))

n11=tclVar("0")
eb1=tkentry(jendela,width="5",textvariable=n11)
tkgrid(tklabel(jendela,text="Banyak Sampel Pertama :"))
tkgrid(eb1)

n22=tclVar("0")
eb2=tkentry(jendela,width="5",textvariable=n22)
tkgrid(tklabel(jendela,text="Banyak Sampel Kedua :"))
tkgrid(eb2)

dval=tclVar("0")
dent=tkentry(jendela,width="5",textvariable=dval)
tkgrid(tklabel(jendela,text="Nilai d0 (d nol) :"))
tkgrid(dent)

#----------------------------------------------------------fungsitabel1

fungsitabel1=function()
{
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
n1=as.numeric(tclvalue(n11))
n2=as.numeric(tclvalue(n22))

#-----------------------------------------------------------------if
  	if(hipo0==" "||hipo1==" ")
    	{tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{
	if(n1==0||n2==0)
	{tkmessageBox(message="ANDA BELUM MENGINPUT BANYAK DATA DENGAN BENAR",icon="warning")}
else
{
#-------------------------------------------------------------jendela1
require(tcltk)
jendela1=tktoplevel()
tktitle(jendela1)<-"Input Sampel"


teks1=tkfont.create(family="sans",weight="bold",size=14)
	tkgrid(tklabel(jendela1,text="MASUKKAN DATA",font=teks1))
	tkgrid(tklabel(jendela1,text=" "))
	tkgrid(tklabel(jendela1,text=" "))
tkgrid(tklabel(jendela1,text="Sampel pertama :                   Sampel kedua :",font=teks1),sticky="n")

#----------------------------------------------------------------input tabel1
tclRequire("Tktable")
bar1=as.numeric(tclvalue(n11))
kol=1
wadah1=matrix(0,bar1,kol)
x<-matrix(0,bar1,kol)
isitabel1= tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-wadah1[i,j]
 }
}
isitabel1[[0,0]]="n1"

tabel1=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel1=tkwidget(jendela1,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")

#---------------------------------------------------------input tabel2
bar2=as.numeric(tclvalue(n22))
kol=1
wadah2=matrix(0,bar2,kol)
y<-matrix(0,bar2,kol)
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-wadah2[i,j]
 }
}
isitabel2[[0,0]]="n2"

tabel2=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel2=tkwidget(jendela1,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jendela,text=as.numeric(tclvalue(slidervalue)))

tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jendela,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")


periksa=function()
{
require(tcltk)
jendela00=tktoplevel()
tktitle(jendela00)="Input Data Pertama"
tclRequire("Tktable")

for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

bar1=as.numeric(tclvalue(n11))
kol=1
isitabel1=tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


bar2=as.numeric(tclvalue(n22))
kol=1
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2=tkwidget(jendela00,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup=tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}


input1=function()    #print nilai fungsi n1
{
 for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

#--------------------------------------------------------program uji t
n=length(x)
b=rep(0,n)
d=matrix(b,n,1)
xb=mean(x)
yb=mean(y)

jumd=0
for(i in 1:n)
{
d[i]=x[i]-y[i]
jumd=jumd+d[i]
}

y=sd(d)
dbar=jumd/n
a=as.numeric(tclvalue(slidervalue))
alfa=a/100
dnol=as.numeric(tclvalue(dval))
v=n-1
t=(dbar-dnol)/(y/sqrt(n))

#----------------------------------------------------------jendela2(hasil)
require(tcltk)
jendela2=tktoplevel()
tkwm.title(jendela2, "HASIL ANALISIS PAIRED TEST")

hasil=function()
{
tkgrid(tklabel(jendela2,text=" "))
tkgrid(tklabel(jendela2,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BERPASANGAN:",font=teks1))
tkgrid(tklabel(jendela2,text="PAIRED TEST (DATA INPUT)",font=teks1))
tkgrid(tklabel(jendela2,text=" "))
tkgrid(tklabel(jendela2,text="HIPOTESIS"))
teks=tktext(jendela2,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela2,text="\nDATA INPUTAN"))
teks2=tktext(jendela2,height="12")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Rata - rata (d bar) :\n                         ")
tkinsert(teks2,"end",dbar)
tkinsert(teks2,"end","\nSimp. Baku (Sd) :\n                         ")
tkinsert(teks2,"end",y)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela2,text="\nDAERAH KRITIS"))
teks3=tktext(jendela2,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3) 
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel) atau T-hitung > T-tabel") 
  tt=qt(1-alfa/2,v)
  if(t>-tt && t<tt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel)")
  tt=qt(1-alfa,v)
  if(t>-tt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung > T-tabel")
  tt=qt(1-alfa,v)
  if(t<tt) a=0
 } 

tkgrid(tklabel(jendela2,text="\nSTATISTIK UJI"))
teks4=tktext(jendela2,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:\n                    ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Statistik tabel:\n                    ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jendela2,text="\nHASIL"))
teks5<- tktext(jendela2,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}

tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombolse<-tkbutton(jendela2,text="SELESAI",command=function()tkdestroy(jendela2),bg="aquamarine")
tkgrid(tombolse,sticky="n")
tkgrid(tklabel(jendela2,text=" "))

}
hasil()
}


tkdestroy(tmblselesai)

tombolperiksa<-tkbutton(jendela,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tkgrid(tklabel(jendela,text="Nilai alfa:"),slidervaluelabel,tklabel(jendela,text="%"))
tkgrid(alfa)

tombolprint<-tkbutton(jendela,text="HASIL ANALISIS",command=input1,bg="gold")
tkgrid(tombolprint,padx=5,pady=5)

selesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(selesai,padx=5,pady=5,sticky="n")

tombolselesai1<-tkbutton(jendela1,text="SELESAI",command=function()tkdestroy(jendela1),bg="aquamarine")
tkgrid(tombolselesai1,sticky="n")
}
} #-----------------------------------------------------tutup if
}


tkgrid(tklabel(jendela,text=" "))
tombolnext<-tkbutton(jendela,text="MASUKKAN DATA",command=fungsitabel1,bg="skyblue")
tkgrid(tombolnext,padx=5,pady=5)

tmblselesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(tmblselesai,padx=5,pady=5,sticky="n")
}



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx




#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx  PAIRED TEST DATA IMPORT  xxxxxxxxxxxxxxxxxxxxxxxxxxxx
asumsi4import=function()
{

require(tcltk)
jdl3=tktoplevel()
tktitle(jdl3)="Data Import SPSS Paired Test"
  

font1=tkfont.create(family="sans",size=14,weight="bold")
font2=tkfont.create(family="times",size=10,weight="bold")
tkgrid(tklabel(jdl3,text="UJI 2 RATA RATA SAMPEL BERPASANGAN",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text="PAIRED TEST (DATA IMPORT)",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text=" "))


hip0=tclVar(" ")
h0=tkentry(jdl3,width="100",textvariable=hip0) 
tkgrid(tklabel(jdl3,text="H0:",font=font2)) 
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jdl3,width="100",textvariable=hip1)
tkgrid(tklabel(jdl3,text="H1:",font=font2))
tkgrid(h1) 
teks3=tclVar()

tkgrid(tklabel(jdl3,text=" "))

rb1=tkradiobutton(jdl3)
rb2=tkradiobutton(jdl3)
rb3=tkradiobutton(jdl3)

rbValue=tclVar("sama")

tkconfigure(rb1,variable=rbValue,value="sama")
tkconfigure(rb2,variable=rbValue,value="kurang")
tkconfigure(rb3,variable=rbValue,value="lebih")

tkgrid(tklabel(jdl3,text="Pilihan hipotesis:",font=font2))
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2 != d0"),rb1)
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2  < d0"),rb2)
tkgrid(tklabel(jdl3,text="H1 : Miu1 - Miu2  > d0"),rb3)
                
tkgrid(tklabel(jdl3,text=" "))
slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jdl3,text=as.numeric(tclvalue(slidervalue)))
tkgrid(tklabel(jdl3,text="Nilai alfa:"),slidervaluelabel,tklabel(jdl3,text="%"))
tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jdl3,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")
tkgrid(alfa)

dval=tclVar("0")
dent=tkentry(jdl3,width="5",textvariable=dval)
tkgrid(tklabel(jdl3,text="Nilai d0 (d nol) :"))
tkgrid(dent)
 
#---------------------------------------------------------program import
fy=function()
{ 
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
if(hipo0==" "||hipo1==" ")
  {tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{  
  library(foreign)
  data1=read.spss(file.choose(),use.value.labels=TRUE,max.value.labels=Inf,to.data.frame=TRUE)


periksa=function()
{
require(tcltk)
jendela00<-tktoplevel()
tktitle(jendela00)<-"Input Sampel"
tclRequire("Tktable")

x=c(data1[,1])
y=c(data1[,2])
x=x[x>0]
y=y[y>0]
n1=length(x)
n2=length(y)

kol<-1
isitabel1<- tclArray()
for (i in 0:n1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(n1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


kol<-1
isitabel2<- tclArray()
for (i in 0:n2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2<-tkwidget(jendela00,"table",variable=isitabel2,rows=(n2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup<-tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}

#-------------------------------------------------------------program uji t
fy1=function()
{    
  a=as.numeric(tclvalue(slidervalue))
  x=c(data1[,1])
  y=c(data1[,2])
  n=length(x)
  b=rep(0,n)
  d=matrix(b,n,1)
  xb=mean(x)
  yb=mean(y)

jumd=0
for(i in 1:n)
{
d[i]=x[i]-y[i]
jumd=jumd+d[i]
}

  yd=sd(d)
  dbar=jumd/n
  dnol=as.numeric(tclvalue(dval))
  alfa=a/100
  v=n-1
  t=(dbar-dnol)/(yd/sqrt(n))

#------------------------------------------------------------jendela (hasil)
require(tcltk)
jendela22 <- tktoplevel()
tkwm.title(jendela22, "HASIL ANALISIS PAIRED TEST")

hasil2=function()
{
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HASIL ANALISIS UJI RATA RATA POPULASI 2 SAMPEL BERPASANGAN:",font=font1))
tkgrid(tklabel(jendela22,text="PAIRED TEST (DATA IMPORT)",font=font1))
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HIPOTESIS"))
teks <- tktext(jendela22,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela22,text="\nDATA IMPORT"))
teks2<- tktext(jendela22,height="11")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel berpasangan :\n                         ")
tkinsert(teks2,"end",n)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Rata - rata (d bar) :\n                         ")
tkinsert(teks2,"end",dbar)
tkinsert(teks2,"end","\nSimp. Baku (Sd) :\n                         ")
tkinsert(teks2,"end",yd)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela22,text="\nDAERAH KRITIS"))
teks3<- tktext(jendela22,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
rbVal=as.character(tclvalue(rbValue))
 if(rbVal=="sama") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel) atau T-hitung > T-tabel") 
  tt=qt(1-alfa/2,v)
  if(t>-tt && t<tt) a=0
 }
 if(rbVal=="kurang") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung < -(T-tabel)")
  tt=qt(1-alfa,v)
  if(t>-tt) a=0
 }
 if(rbVal=="lebih") 
 {
  tkinsert(teks3,"end","Tolak H0 jika nilai T-hitung > T-tabel")
  tt=qt(1-alfa,v)
  if(t<tt) a=0
 }

tkgrid(tklabel(jendela22,text="\nSTATISTIK UJI"))
teks4<- tktext(jendela22,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Statistik hitung:\n                    ")
tkinsert(teks4,"end",t)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Statistik tabel:\n                    ")
tkinsert(teks4,"end",tt)

tkgrid(tklabel(jendela22,text="\nHASIL"))
teks5<- tktext(jendela22,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if(a==0)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}
tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombols<-tkbutton(jendela22,text="SELESAI",command=function()tkdestroy(jendela22),bg="aquamarine")
tkgrid(tombols)
}
hasil2()
}

tkdestroy(kb)

tombolperiksa<-tkbutton(jdl3,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tbm=tkbutton(jdl3,text="HASIL ANALISIS",command=fy1,bg="gold")
tkgrid(tbm)   

back=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(back,sticky="n",padx=5,pady=5)
}#-------------------------------------------------------------------- tutup if 
}

tkgrid(tklabel(jdl3,text=" "))
mb=tkbutton(jdl3,text="IMPORT",command=fy,bg="skyblue")
tkgrid(mb,sticky="n",padx=5,pady=5)

kb=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(kb,sticky="n",padx=5,pady=5)

tkgrid(tklabel(jdl3,text=" "))
 tkgrid(tklabel(jdl3,text="*Catatan : Data Import hanya bisa dengan format *sav (SPSS)"))
tkgrid(tklabel(jdl3,text=" "))
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx




#xxxxxxxxxxxxxxxxxxxxxxxxxx  MAN WHITNEYY INPUT xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
mwinput=function()
{
#----------------------------------------jendela input data mann whitney
require(tcltk)
jendela=tktoplevel()
tktitle(jendela)<-"Data Input Uji Mann Whitney"

teksa=tkfont.create(family="sans",weight="bold",size=10)
teksb=tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jendela,text="UJI MANN WHITNEY",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text="WILCOXON (INPUT DATA)",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text=" "))

hip0=tclVar(" ")
h0=tkentry(jendela,width="100",textvariable=hip0)
tkgrid(tklabel(jendela,text="H0:",font=teksa))
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jendela,width="100",textvariable=hip1)
tkgrid(tklabel(jendela,text="H1:",font=teksa))
tkgrid(h1)

tkgrid(tklabel(jendela,text=" "))

n11=tclVar("0")
eb1=tkentry(jendela,width="5",textvariable=n11)
tkgrid(tklabel(jendela,text="Banyak Sampel Pertama:"))
tkgrid(eb1)

n22=tclVar("0")
eb2=tkentry(jendela,width="5",textvariable=n22)
tkgrid(tklabel(jendela,text="Banyak Sampel Kedua:"))
tkgrid(eb2)


#----------------------------------------------------------fungsitabel1

fungsitabel1=function()
{
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
n1=as.numeric(tclvalue(n11))
n2=as.numeric(tclvalue(n22))

#------------------------------------------------------------------if
  if(hipo0==" "||hipo1==" ")
  {tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{
  if(n1==0||n2==0)
  {tkmessageBox(message="ANDA BELUM MENGINPUT BANYAK DATA DENGAN BENAR",icon="warning")}
else
{
#-------------------------------------------------------------jendela1
require(tcltk)
jendela1=tktoplevel()
tktitle(jendela1)<-"input Sampel"


teks1=tkfont.create(family="sans",weight="bold",size=14)
	tkgrid(tklabel(jendela1,text="MASUKKAN DATA",font=teks1))
	tkgrid(tklabel(jendela1,text=" "))
	tkgrid(tklabel(jendela1,text=" "))
tkgrid(tklabel(jendela1,text="Sampel pertama :                   Sampel kedua :",font=teks1),sticky="n")

#----------------------------------------------------------------input tabel1
tclRequire("Tktable")
bar1=as.numeric(tclvalue(n11))
kol=1
wadah1=matrix(0,bar1,kol)
x<-matrix(0,bar1,kol)
isitabel1= tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-wadah1[i,j]
 }
}
isitabel1[[0,0]]="n1"

tabel1=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel1=tkwidget(jendela1,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")

#---------------------------------------------------------input tabel2
bar2=as.numeric(tclvalue(n22))
kol=1
wadah2=matrix(0,bar2,kol)
y<-matrix(0,bar2,kol)
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-wadah2[i,j]
 }
}
isitabel2[[0,0]]="n2"

tabel2=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel2=tkwidget(jendela1,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jendela,text=as.numeric(tclvalue(slidervalue)))

tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jendela,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")


periksa=function()
{
require(tcltk)
jendela00=tktoplevel()
tktitle(jendela00)="input Sampel"
tclRequire("Tktable")

for(i in 1:bar1)
	{
      for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

bar1=as.numeric(tclvalue(n11))
kol=1
isitabel1=tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


bar2=as.numeric(tclvalue(n22))
kol=1
isitabel2=tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2=tkwidget(jendela00,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup=tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}


input1=function()    #print nilai fungsi n1
{
 for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

#--------------------------------------------------------program uji z
  a=as.numeric(tclvalue(slidervalue))
  x=x[x>0]
  y=y[y>0]

  n1=length(x)
  n2=length(y)

  alfa=a/100
  
  a=wilcox.test(x,y)
  b=wilcox.test(y,x)
  a1=as.numeric(a[1])
  b1=as.numeric(b[1])
  if(a1<b1)
  {
   U=a1
   pv=as.numeric(a[3]) 
  }
  if(b1<a1)
  {
   U=b1
   pv=as.numeric(b[3])
  }
  
  if(n1<20 && n2<20)
  {
    pval=pv
  }
  
  if(n1>20 || n2>20)
  {
    z1=U-(n1*n2/2)
    z2=((n1*n2)*(n1+n2+1))/12
    z=z1/sqrt(z2)
    pval=pnorm(z,0,1)
  }

#----------------------------------------------------------jendela2(hasil)
require(tcltk)
jendela2=tktoplevel()
tkwm.title(jendela2, "HASIL")

hasil=function()
{
tkgrid(tklabel(jendela2,text=" "))
tkgrid(tklabel(jendela2,text="HASIL ANALISIS UJI MANN WHITNEY:",font=teksb))
tkgrid(tklabel(jendela2,text="WILCOXON (INPUT DATA)",font=teksb))
tkgrid(tklabel(jendela2,text=" "))
tkgrid(tklabel(jendela2,text="HIPOTESIS"))
teks=tktext(jendela2,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela2,text="\nDATA INPUTAN"))
teks2=tktext(jendela2,height="7")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela2,text="\nDAERAH KRITIS"))
teks3=tktext(jendela2,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
tkinsert(teks3,"end","Tolak H0 jika nilai P-Value < Alfa") 

tkgrid(tklabel(jendela2,text="\nSTATISTIK UJI"))
teks4=tktext(jendela2,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Nilai P-Value:\n                    ")
tkinsert(teks4,"end",pval)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Nilai Alfa:\n                    ")
tkinsert(teks4,"end",alfa)

tkgrid(tklabel(jendela2,text="\nHASIL"))
teks5<- tktext(jendela2,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)


if (pval > alfa)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}

tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombolse<-tkbutton(jendela2,text="SELESAI",command=function()tkdestroy(jendela2),bg="aquamarine")
tkgrid(tombolse,sticky="n")
tkgrid(tklabel(jendela2,text=" "))

}
hasil()
}
}

tkdestroy(tmblselesai)


tombolperiksa<-tkbutton(jendela,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tkgrid(tklabel(jendela,text="Nilai alfa:"),slidervaluelabel,tklabel(jendela,text="%"))
tkgrid(alfa)

tombolprint<-tkbutton(jendela,text="HASIL ANALISIS",command=input1,bg="gold")
tkgrid(tombolprint,padx=5,pady=5)

selesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(selesai,padx=5,pady=5,sticky="n")

tombolselesai1<-tkbutton(jendela1,text="SELESAI",command=function()tkdestroy(jendela1),bg="aquamarine")
tkgrid(tombolselesai1,sticky="n")
}
} #-----------------------------------------------------tutup fungsi tabel 1


tkgrid(tklabel(jendela,text=" "))
tombolnext<-tkbutton(jendela,text="MASUKKAN DATA",command=fungsitabel1,bg="skyblue")
tkgrid(tombolnext,padx=5,pady=5)

tmblselesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(tmblselesai,padx=5,pady=5,sticky="n")
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx




#xxxxxxxxxxxxxxxxxxxxxxxx  MAN WHITNEY IMPORT  xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
mwimport=function()
{

require(tcltk)
jdl3=tktoplevel()
tktitle(jdl3)="DATA IMPORT SPSS"
  

font1=tkfont.create(family="sans",size=14,weight="bold")
font2=tkfont.create(family="times",size=10,weight="bold")
tkgrid(tklabel(jdl3,text="UJI MANN WHITNEY",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text="WILCOXON (DATA IMPORT)",font=font1),sticky="n")
tkgrid(tklabel(jdl3,text=" "))


hip0=tclVar(" ")
h0=tkentry(jdl3,width="100",textvariable=hip0) 
tkgrid(tklabel(jdl3,text="H0:",font=font2)) 
tkgrid(h0)

hip1=tclVar(" ")
h1=tkentry(jdl3,width="100",textvariable=hip1)
tkgrid(tklabel(jdl3,text="H1:",font=font2))
tkgrid(h1) 
teks3=tclVar()
                
tkgrid(tklabel(jdl3,text=" "))
slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jdl3,text=as.numeric(tclvalue(slidervalue)))
tkgrid(tklabel(jdl3,text="Nilai alfa:"),slidervaluelabel,tklabel(jdl3,text="%"))
tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jdl3,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")
tkgrid(alfa)

#---------------------------------------------------------program import
fy=function()
{ 
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
if(hipo0==" "||hipo1==" ")
  {tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{  
  library(foreign)
  data1=read.spss(file.choose(),use.value.labels=TRUE,max.value.labels=Inf,to.data.frame=TRUE)

periksa=function()
{
require(tcltk)
jendela00<-tktoplevel()
tktitle(jendela00)<-"input data pertama"
tclRequire("Tktable")

x=c(data1[,1])
y=c(data1[,2])
x=x[x>0]
y=y[y>0]
n1=length(x)
n2=length(y)

kol<-1
isitabel1<- tclArray()
for (i in 0:n1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(n1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


kol<-1
isitabel2<- tclArray()
for (i in 0:n2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2<-tkwidget(jendela00,"table",variable=isitabel2,rows=(n2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup<-tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}

#-------------------------------------------------------------program uji z
fy1=function()
{    
  a=as.numeric(tclvalue(slidervalue))
  x=c(data1[,1])
  y=c(data1[,2])
  x=x[x>0]
  y=y[y>0]

  n1=length(x)
  n2=length(y)

  alfa=a/100
	
  a=wilcox.test(x,y)
  b=wilcox.test(y,x)
  a1=as.numeric(a[1])
  b1=as.numeric(b[1])
  if(a1<b1)
  {
   U=a1
   pv=as.numeric(a[3]) 
  }
  if(b1<a1)
  {
   U=b1
   pv=as.numeric(b[3])
  }
  
  if(n1<20 && n2<20)
  {
    pval=pv
  }
  
  if(n1>20 || n2>20)
  {
    z1=U-(n1*n2/2)
    z2=((n1*n2)*(n1+n2+1))/12
    z=z1/sqrt(z2)
    pval=pnorm(z,0,1)
  }


#------------------------------------------------------------jendela (hasil)
require(tcltk)
jendela22 <- tktoplevel()
tkwm.title(jendela22, "HASIL")

hasil2=function()
{
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HASIL ANALISIS UJI MANN WHITNEY:",font=font1))
tkgrid(tklabel(jendela22,text="WILCOXON (DATA IMPORT)",font=font1))
tkgrid(tklabel(jendela22,text=" "))
tkgrid(tklabel(jendela22,text="HIPOTESIS"))
teks <- tktext(jendela22,height="4")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","H0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n\n")

tkgrid(tklabel(jendela22,text="\nDATA IMPORT"))
teks2<- tktext(jendela22,height="7")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)
tkinsert(teks2,"end","Banyak sampel pertama :\n                         ")
tkinsert(teks2,"end",n1)
tkinsert(teks2,"end","\nBanyak sampel kedua:\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")
tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkgrid(tklabel(jendela22,text="\nDAERAH KRITIS"))
teks3<- tktext(jendela22,height="1")
tkmark.set(teks3,"insert","0.0")
tkgrid(teks3)
tkinsert(teks3,"end","Tolak H0 jika nilai P-Value < Alfa") 


tkgrid(tklabel(jendela22,text="\nSTATISTIK UJI"))
teks4<- tktext(jendela22,height="4")
tkmark.set(teks4,"insert","0.0")
tkgrid(teks4)
tkinsert(teks4,"end","Nilai P-Value:\n                    ")
tkinsert(teks4,"end",pval)
tkinsert(teks4,"end","\n")
tkinsert(teks4,"end","Nilai Alfa:\n                    ")
tkinsert(teks4,"end",alfa)

tkgrid(tklabel(jendela22,text="\nHASIL"))
teks5<- tktext(jendela22,height="4")
tkmark.set(teks5,"insert","0.0")
tkgrid(teks5)

if (pval>alfa)
{
tkinsert(teks5,"end","Keputusan: Terima H0\nKesimpulan:")
tkinsert(teks5,"end",hipo0)
}
else
{
tkinsert(teks5,"end","Keputusan: Tolak H0\nKesimpulan:")
tkinsert(teks5,"end",hipo1)
}
tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkconfigure(teks3, state="normal")
tkconfigure(teks4, state="normal")
tkconfigure(teks5, state="normal")
tkfocus(teks)

tombols<-tkbutton(jendela22,text="SELESAI",command=function()tkdestroy(jendela22),bg="aquamarine")
tkgrid(tombols)
}
hasil2()
}

tkdestroy(kb)

tombolperiksa<-tkbutton(jdl3,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tbm=tkbutton(jdl3,text="HASIL ANALISIS",command=fy1,bg="gold")
tkgrid(tbm)   

back=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(back,sticky="n",padx=5,pady=5)
}#--------------------------------------------------------------------fy1
}

tkgrid(tklabel(jdl3,text=" "))
mb=tkbutton(jdl3,text="IMPORT",command=fy,bg="skyblue")
tkgrid(mb,sticky="n",padx=5,pady=5)

kb=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(kb,sticky="n",padx=5,pady=5)

tkgrid(tklabel(jdl3,text=" "))
 tkgrid(tklabel(jdl3,text="*Catatan : Data Import hanya bisa dengan format *sav (SPSS)"))
tkgrid(tklabel(jdl3,text=" "))
#-------------------------------------------------------------------fy

}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx






#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
normalinput=function()
{
require(tcltk)
jendela<-tktoplevel()
tktitle(jendela)<-"Uji Normalitas: Shapiro Wilk (Input Data)"

teksa<-tkfont.create(family="sans",weight="bold",size=10)
teksb<-tkfont.create(family="sans",weight="bold",size=14)
tkgrid(tklabel(jendela,text="UJI NORMALITAS: SHAPIRO WILK",font=teksb),sticky="n")
tkgrid(tklabel(jendela,text="(INPUT DATA)",font=teksb),sticky="n")

tkgrid(tklabel(jendela,text=" "))

hip0<-tclVar("Data berdistribusi normal")
h0<-tkentry(jendela,width="100",textvariable=hip0)
tkgrid(tklabel(jendela,text="H0:",font=teksa))
tkgrid(h0)

hip1<-tclVar("Data tidak berdistribusi normal")
h1<-tkentry(jendela,width="100",textvariable=hip1)
tkgrid(tklabel(jendela,text="H1:",font=teksa))
tkgrid(h1)

tkgrid(tklabel(jendela,text=" "))



tkgrid(tklabel(jendela,text=" "))

n11<-tclVar("0")
eb1<-tkentry(jendela,width="5",textvariable=n11)
tkgrid(tklabel(jendela,text="Banyak Data Pertama:"))
tkgrid(eb1)

n22<-tclVar("0")
eb2<-tkentry(jendela,width="5",textvariable=n22)
tkgrid(tklabel(jendela,text="Banyak Data Kedua:"))
tkgrid(eb2)


#----------------------------------------------------------fungsitabel1

fungsitabel1=function()
{
hipo0=as.character(tclvalue(hip0))
hipo1=as.character(tclvalue(hip1))
n1=as.numeric(tclvalue(n11))
n2=as.numeric(tclvalue(n22))

#-----------------------------------------------------------------if
  	if(hipo0==" "||hipo1==" ")
    	{tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{
		if(n1<3||n2>5000)
		{tkmessageBox(message="ANDA BELUM MENGINPUT BANYAK DATA DENGAN BENAR",icon="warning")}
else
{
#-------------------------------------------------------------jendela1
require(tcltk)
jendela1<-tktoplevel()
tktitle(jendela1)<-"input Data"


teks1<-tkfont.create(family="sans",weight="bold",size=14)
	tkgrid(tklabel(jendela1,text="MASUKKAN DATA",font=teks1))
	tkgrid(tklabel(jendela1,text=" "))
	tkgrid(tklabel(jendela1,text=" "))
tkgrid(tklabel(jendela1,text="Data pertama :                   Data kedua :",font=teks1),sticky="n")

#----------------------------------------------------------------input tabel1
tclRequire("Tktable")
bar1<-as.numeric(tclvalue(n11))
kol<-1
wadah1<-matrix(0,bar1,kol)
x<-matrix(0,bar1,kol)
isitabel1<- tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-wadah1[i,j]
 }
}
isitabel1[[0,0]]="n1"

tabel1=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel1<-tkwidget(jendela1,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")

#---------------------------------------------------------input tabel2
bar2<-as.numeric(tclvalue(n22))
kol<-1
wadah2<-matrix(0,bar2,kol)
y<-matrix(0,bar2,kol)
isitabel2<- tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-wadah2[i,j]
 }
}
isitabel2[[0,0]]="n2"

tabel2=read.delim(file="clipboard", header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE)

tabel2<-tkwidget(jendela1,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightblue")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jendela,text=as.numeric(tclvalue(slidervalue)))

tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jendela,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")


periksa=function()
{
require(tcltk)
jendela00<-tktoplevel()
tktitle(jendela00)<-"input Data"
tclRequire("Tktable")

for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)

bar1<-as.numeric(tclvalue(n11))
kol<-1
isitabel1<- tclArray()
for (i in 0:bar1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(bar1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


bar2<-as.numeric(tclvalue(n22))
kol<-1
isitabel2<- tclArray()
for (i in 0:bar2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2<-tkwidget(jendela00,"table",variable=isitabel2,rows=(bar2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup<-tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}


input1<-function()    #print nilai fungsi n1
{
 for(i in 1:bar1)
	{
   for(j in 1:kol)
	{
	x[i,j]<-as.numeric(isitabel1[[i,j-1]])
	}
	}
	x=as.numeric(x)


for(i in 1:bar2)
	{
	for(j in 1:kol)
	{
	y[i,j]<-as.numeric(isitabel2[[i,j-1]])
	}
	}
	y=as.numeric(y)




#--------------------------------------------------------program shapiro
a=as.numeric(tclvalue(slidervalue))
alfa=a/100
s1=shapiro.test(x)
pv1<-as.numeric(s1[2])
s2=shapiro.test(y)
pv2<-as.numeric(s2[2])


plotxin=function()
{
win.graph()
qqnorm(x)
qqline(x)
}

plotyin=function()
{
win.graph()
qqnorm(y)
qqline(y)
}



#----------------------------------------------------------jendela2(hasil)
require(tcltk)
jendela2 <- tktoplevel()
tkwm.title(jendela2, "HASIL")

hasil=function()
{
tkgrid(tklabel(jendela2,text="UJI NORMALITAS: SHAPIRO WILK",font=teks1))
tkgrid(tklabel(jendela2,text="DATA PERTAMA"))
teks <- tktext(jendela2,height="16")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","Hipotesis:\nH0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n")


tkinsert(teks,"end","Banyak Data :\n                         ")
tkinsert(teks,"end",n1)
tkinsert(teks,"end","\n\n")

tkinsert(teks,"end","(Daerah Kritis: ")
tkinsert(teks,"end","Tolak H0 jika P-value < nilai alfa)\n\n")

tkinsert(teks,"end","Nilai alfa:\n                         ")
tkinsert(teks,"end",alfa)

tkinsert(teks,"end","\nP-value:\n                    ")
tkinsert(teks,"end",pv1)
tkinsert(teks,"end","\n\n")

if (pv1>=alfa)
{
tkinsert(teks,"end","Keputusan: Terima H0\nKesimpulan: ")
tkinsert(teks,"end",hipo0)
p1=0
}
else
{
tkinsert(teks,"end","Keputusan: Tolak H0\nKesimpulan: ")
tkinsert(teks,"end",hipo1)
p1=1
}

plot1<-tkbutton(jendela2,text="PLOT NORMALITAS DATA 1",command=plotxin,bg="coral")
tkgrid(plot1,sticky="n")

tkgrid(tklabel(jendela2,text="\nDATA KEDUA"))
teks2<- tktext(jendela2,height="16")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)

tkinsert(teks2,"end","Hipotesis:\nH0: ")
tkinsert(teks2,"end",hipo0)
tkinsert(teks2,"end","\n")
tkinsert(teks2,"end","H1: ")
tkinsert(teks2,"end",hipo1)
tkinsert(teks2,"end","\n\n")


tkinsert(teks2,"end","Banyak Data :\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")

tkinsert(teks2,"end","(Daerah Kritis: ")
tkinsert(teks2,"end","Tolak H0 jika P-value < nilai alfa)\n\n")

tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkinsert(teks2,"end","\nP-value:\n                    ")
tkinsert(teks2,"end",pv2)
tkinsert(teks2,"end","\n\n")

if (pv2>=alfa)
{
tkinsert(teks2,"end","Keputusan: Terima H0\nKesimpulan: ")
tkinsert(teks2,"end",hipo0)
p2=0
}
else
{
tkinsert(teks2,"end","Keputusan: Tolak H0\nKesimpulan: ")
tkinsert(teks2,"end",hipo1)
p2=1
}


tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkfocus(teks)

plot2<-tkbutton(jendela2,text="PLOT NORMALITAS DATA 2",command=plotyin,bg="coral")
tkgrid(plot2,sticky="n")

if (p1==0 && p2==0) {tkmessageBox(message="DATA INI MENGGUNAKAN METODE PARAMETRIK",icon="info")}
else {tkmessageBox(message="DATA INI MENGGUNAKAN METODE NON-PARAMETRIK",icon="info")}

tombolse<-tkbutton(jendela2,text="SELESAI",command=function()tkdestroy(jendela2),bg="aquamarine")
tkgrid(tombolse,sticky="n")

}
hasil()

#--------------------------------------------------
}

tkdestroy(tmblselesai)


tombolperiksa<-tkbutton(jendela,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)

tkgrid(tklabel(jendela,text="Nilai alfa:"),slidervaluelabel,tklabel(jendela,text="%"))
tkgrid(alfa)

tombolprint<-tkbutton(jendela,text="HASIL ANALISIS",command=input1,bg="gold")
tkgrid(tombolprint,padx=5,pady=5)

selesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(selesai,padx=5,pady=5,sticky="n")

tlselesai1<-tkbutton(jendela1,text="SELESAI",command=function()tkdestroy(jendela1),bg="aquamarine")
tkgrid(tlselesai1,padx=5,pady=5,sticky="n")
}
} #-----------------------------------------------------tutup if
}


tkgrid(tklabel(jendela,text=" "))
tombolnext<-tkbutton(jendela,text="MASUKKAN DATA",command=fungsitabel1,bg="skyblue")
tkgrid(tombolnext,padx=5,pady=5)

tmblselesai<-tkbutton(jendela,text="KEMBALI KE MENU",command=function()tkdestroy(jendela),bg="turquoise")
tkgrid(tmblselesai,padx=5,pady=5,sticky="n")
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx







#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
normalimport=function()
{
  require(tcltk)
  jdl3=tktoplevel()
  tktitle(jdl3)="DATA IMPORT"
  

  font1=tkfont.create(family="sans",size=14,weight="bold")
  font2=tkfont.create(family="times",size=14)
  tkgrid(tklabel(jdl3,text="UJI NORMALITAS: SHAPIRO WILK",font=font1),sticky="n")
  tkgrid(tklabel(jdl3,text="(DATA IMPORT)\n",font=font1),sticky="n")

  hip0=tclVar("Data berdistribusi normal")
  hip1=tclVar("Data tidak berdistribusi normal")
  teks3=tclVar()

  h0=tkentry(jdl3,width="100",textvariable=hip0)
  h1=tkentry(jdl3,width="100",textvariable=hip1)
  tkgrid(tklabel(jdl3,text="H0:",font=font2))
  tkgrid(h0)
  tkgrid(tklabel(jdl3,text="H1:",font=font2))
  tkgrid(h1)                 


tkgrid(tklabel(jdl3,text=" "))
slidervalue=tclVar("5.0")
slidervaluelabel=tklabel(jdl3,text=as.numeric(tclvalue(slidervalue)))
tkgrid(tklabel(jdl3,text="Nilai alfa:"),slidervaluelabel,tklabel(jdl3,text="%"))
tkconfigure(slidervaluelabel,textvariable=slidervalue)
alfa=tkscale(jdl3,from=1,to=20,showvalue=T,variable=slidervalue,resolution=0.1,orient="horizontal")
tkgrid(alfa)
 
#---------------------------------------------------------program import
fy=function()
{ #---------------------------------------------------fy
	 hipo0=as.character(tclvalue(hip0))
       hipo1=as.character(tclvalue(hip1))
if(hipo0==" "||hipo1==" ")
    	{tkmessageBox(message="ANDA BELUM MENGINPUT HIPOTESIS DENGAN BENAR",icon="warning")}
else
{  
       library(foreign)
       data1=read.spss(file.choose(),use.value.labels=TRUE,max.value.labels=Inf,to.data.frame=TRUE)

periksa=function()
{
require(tcltk)
jendela00<-tktoplevel()
tktitle(jendela00)<-"input data pertama"
tclRequire("Tktable")

x=c(data1[,1])
y=c(data1[,2])
	 x=x[x>0]
	 y=y[y>0]
	 n1=length(x)
	 n2=length(y)

kol<-1
isitabel1<- tclArray()
for (i in 0:n1)
{
for (j in 0:kol)
 {
 isitabel1[[i,j]]<-x[i]
 }
}
isitabel1[[0,0]]="n1"

tabel1<-tkwidget(jendela00,"table",variable=isitabel1,rows=(n1+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")


kol<-1
isitabel2<- tclArray()
for (i in 0:n2)
{
for (j in 0:kol)
 {
 isitabel2[[i,j]]<-y[i]
 }
}
isitabel2[[0,0]]="n2"

tabel2<-tkwidget(jendela00,"table",variable=isitabel2,rows=(n2+1),cols=(kol),titlerows=1,selectmode="extended  ",colwidth=10,background="lightseagreen")
tkconfigure(tabel2,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
tkgrid(tabel1,tabel2,sticky="n")

tomboltutup<-tkbutton(jendela00,text="TUTUP",command=function()tkdestroy(jendela00),bg="aquamarine")
tkgrid(tomboltutup,sticky="e")

}


#............................................................................................
fy1=function()
{    #f-------------------------------------------------y1

  a=as.numeric(tclvalue(slidervalue))
  x=c(data1[,1])
  y=c(data1[,2])
  x=x[x>0]
  y=y[y>0]
  n1=length(x)
  n2=length(y)
  alfa=a/100
	 
  s1=shapiro.test(x)
  pv1<-as.numeric(s1[2])

  s2=shapiro.test(y)
  pv2<-as.numeric(s2[2])

plotxims=function()
{
win.graph()
qqnorm(x)
qqline(x)
}

plotyims=function()
{
win.graph()
qqnorm(y)
qqline(y)
}


#--------------------------------
require(tcltk)
jendela22 <- tktoplevel()
tkwm.title(jendela22, "HASIL")

hasil2=function()
{
tkgrid(tklabel(jendela22,text="UJI NORMALITAS: SHAPIRO WILK",font=font1))
tkgrid(tklabel(jendela22,text="DATA PERTAMA"))
teks <- tktext(jendela22,height="16")
tkgrid(teks)
tkmark.set(teks,"insert","0.0")
tkinsert(teks,"end","Hipotesis:\nH0: ")
tkinsert(teks,"end",hipo0)
tkinsert(teks,"end","\n")
tkinsert(teks,"end","H1: ")
tkinsert(teks,"end",hipo1)
tkinsert(teks,"end","\n\n")


tkinsert(teks,"end","Banyak Data :\n                         ")
tkinsert(teks,"end",n1)
tkinsert(teks,"end","\n\n")

tkinsert(teks,"end","(Daerah Kritis: ")
tkinsert(teks,"end","Tolak H0 jika P-value < nilai alfa)\n\n")

tkinsert(teks,"end","Nilai alfa:\n                         ")
tkinsert(teks,"end",alfa)

tkinsert(teks,"end","\nP-value:\n                    ")
tkinsert(teks,"end",pv1)
tkinsert(teks,"end","\n\n")

if (pv1>=alfa)
{
tkinsert(teks,"end","Keputusan: Terima H0\nKesimpulan: ")
tkinsert(teks,"end",hipo0)
p1=0
}
else
{
tkinsert(teks,"end","Keputusan: Terima H1\nKesimpulan: ")
tkinsert(teks,"end",hipo1)
p1=1
}

plot1<-tkbutton(jendela22,text="PLOT NORMALITAS DATA 1",command=plotxims,bg="coral")
tkgrid(plot1,sticky="n")

tkgrid(tklabel(jendela22,text="\nDATA KEDUA"))
teks2<- tktext(jendela22,height="16")
tkmark.set(teks2,"insert","0.0")
tkgrid(teks2)

tkinsert(teks2,"end","Hipotesis:\nH0: ")
tkinsert(teks2,"end",hipo0)
tkinsert(teks2,"end","\n")
tkinsert(teks2,"end","H1: ")
tkinsert(teks2,"end",hipo1)
tkinsert(teks2,"end","\n\n")


tkinsert(teks2,"end","Banyak Data :\n                         ")
tkinsert(teks2,"end",n2)
tkinsert(teks2,"end","\n\n")

tkinsert(teks2,"end","(Daerah Kritis: ")
tkinsert(teks2,"end","Tolak H0 jika P-value < nilai alfa)\n\n")

tkinsert(teks2,"end","Nilai alfa:\n                         ")
tkinsert(teks2,"end",alfa)

tkinsert(teks2,"end","\nP-value:\n                    ")
tkinsert(teks2,"end",pv2)
tkinsert(teks2,"end","\n\n")

if (pv2>=alfa)
{
tkinsert(teks2,"end","Keputusan: Terima H0\nKesimpulan: ")
tkinsert(teks2,"end",hipo0)
p2=0
}
else
{
tkinsert(teks2,"end","Keputusan: Terima H1\nKesimpulan: ")
tkinsert(teks2,"end",hipo1)
p2=1
}


tkconfigure(teks, state="normal")
tkconfigure(teks2, state="normal")
tkfocus(teks)

plot2<-tkbutton(jendela22,text="PLOT NORMALITAS DATA 2",command=plotyims,bg="coral")
tkgrid(plot2,sticky="n")

tombolse<-tkbutton(jendela22,text="SELESAI",command=function()tkdestroy(jendela22),bg="aquamarine")
tkgrid(tombolse,sticky="n")

if (p1==0 && p2==0) {tkmessageBox(message="DATA INI MENGGUNAKAN METODE PARAMETRIK",icon="info")}
else {tkmessageBox(message="DATA INI MENGGUNAKAN METODE NON-PARAMETRIK",icon="info")}

tkgrid(tklabel(jendela22,text=" "))

}
hasil2()
}

tkdestroy(kb)

tombolperiksa<-tkbutton(jdl3,text="LIHAT DATA",command=periksa,bg="skyblue")
tkgrid(tombolperiksa,padx=5,pady=5)


     tbm=tkbutton(jdl3,text="HASIL ANALISIS",command=fy1,bg="gold")
     tkgrid(tbm)   

back=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(back,sticky="n",padx=5,pady=5)
}
} #--------------------------------------------------------------------

tkgrid(tklabel(jdl3,text=" "))
mb=tkbutton(jdl3,text="IMPORT",command=fy,bg="skyblue")
tkgrid(mb,sticky="n",padx=5,pady=5)

kb=tkbutton(jdl3,text="KEMBALI KE MENU",command=function()tkdestroy(jdl3),bg="turquoise")
tkgrid(kb,sticky="n",padx=5,pady=5)

tkgrid(tklabel(jdl3,text=" "))
 tkgrid(tklabel(jdl3,text="*Catatan : Data Import hanya bisa dengan format *sav (SPSS)"))
tkgrid(tklabel(jdl3,text=" "))  
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx





#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx DESKRIPSI xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
deskripsi=function()
{
	require(tcltk)
	desk=tktoplevel()
	tktitle(desk)="DESKRIPSI ANALISIS UJI"
	font1=tkfont.create(family="sans",size=20,weight="bold")
	font2=tkfont.create(family="times",size=12)
	font3=tkfont.create(family="times",size=14,weight="bold")
	font4=tkfont.create(family="times",size=12,weight="bold")
	

	tkgrid(tklabel(desk,text="DESKRIPSI UJI NORMALITAS : SHAPIRO WILK",font=font3),sticky="n")
	tkgrid(tklabel(desk,text=" "))
	tkgrid(tklabel(desk,text="Uji Shapiro Wilk adalah sebuah metode atau rumus perhitungan sebaran data yang dibuat oleh shapiro dan wilk.",font=font2),sticky="w") 
	tkgrid(tklabel(desk,text="Metode shapiro wilk adalah metode uji normalitas yang efektif dan valid digunakan untuk sampel berukuran 3-5000",font=font2),sticky="w") 
	tkgrid(tklabel(desk,text=" "))

 	tkgrid(tklabel(desk,text="DESKRIPSI UJI NON PARAMETRIK : MANN-WHITNEY",font=font3),sticky="n")
	tkgrid(tklabel(desk,text=" "))
	tkgrid(tklabel(desk,text="Uji Mann Whitney adalah uji non parametrik yang digunakan untuk mengetahui perbedaan median 2 kelompok bebas apabila data variabel terikatnya ordinal atau interval atau rasio tetapi tidak berdistribusi normal.",font=font2),sticky="w") 
	tkgrid(tklabel(desk,text="Uji ini dikembangkan oleh H.B Mann dan D.R Whitney pada tahun1947.",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="Uji ini digunakan sebagai alternatif lain dari uji T-parametrik bila anggapan yang diperlukan bagi uji T tidak dijumpai atau manakala pengukuran dalam penelitiannya lebih lemah dari skala interval",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="Uji ini dipakai untuk mengetes signifikasi perbedaan antara dua populasi, dengan menggunakan sampel random yang ditarik daripopulasi yang sama.",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="Uji ini khusus untuk dua sampel yang independen",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="Apabila n<20 maka menggunakan Uji Man Whitney dan apabila n>20 maka menggunakan Uji Wilcoxon Rank-Sum .",font=font2),sticky="w")
	tkgrid(tklabel(desk,text=" "))

 	tkgrid(tklabel(desk,text="DESKRIPSI UJI PARAMETRIK",font=font3),sticky="n")
	tkgrid(tklabel(desk,text="A. Uji 2 Mean Populasi Saling Bebas",font=font4),sticky="w")
	tkgrid(tklabel(desk,text=" "))
	tkgrid(tklabel(desk,text="   Uji Z adalah uji parametrik yang dapat digunakan untuk menguji rata-rata populasi dua sampel saling bebas, dengan variansi populasi diketahui dan sampel berukuran besar n>30.",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="   Uji T adalah uji parametrik yang dapat digunakan untuk menguji rata-rata populasi dua sampel saling bebas yang terdiri dari 2 bentuk : ",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="	   1. Variansi populasi dianggap sama dan tidak diketahui.",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="	   2. Variansi populasi dianggap tidak sama dan tidak diketahui.",font=font2),sticky="w")
      tkgrid(tklabel(desk,text=" "))
	tkgrid(tklabel(desk,text="B. Uji 2 Mean Populasi Berpasangan (Paired Test)",font=font4),sticky="w")
      tkgrid(tklabel(desk,text=" "))
      tkgrid(tklabel(desk,text="   Uji Paired merupakan uji parametrik yang menggunakan T test untuk menganalisis data berpasangan.",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="   Uji Paired merupakan pengujian yang digunakan untuk membandingkan selisih 2 mean dari sampel berpasangan.",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="   Uji ini harus memiliki sampel berpasangan yang berasal dari subjek sama namun, setiap variabel diambil disaat situasi berbeda.",font=font2),sticky="w")
	tkgrid(tklabel(desk,text="   Uji ini memiliki asumsi data berdistribusi normal.",font=font2),sticky="w")


	tkgrid(tklabel(desk,text=" "))
	kb1=tkbutton(desk,text="KEMBALI",command=function()tkdestroy(desk),bg="lightviolet")
	tkgrid(kb1)
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx





#xxxxxxxxxxxxxxxxxxxxxxxxxxxxx PENGGUNAAN xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
penggunaan=function()
{

 require(tcltk)
 guna=tktoplevel()
 tktitle(guna)="PENGGUNAAN UJI RATA-RATA DUA POPULASI PADA PROGRAM"
 font1=tkfont.create(family="sans",size=20,weight="bold")
 font2=tkfont.create(family="times",size=10)
 font3=tkfont.create(family="times",size=12,weight="bold")

 tkgrid(tklabel(guna,text="BANTUAN PENGGUNAAN PROGRAM",font=font3),sticky="n")
 tkgrid(tklabel(guna,text=" ")) 

 tkgrid(tklabel(guna,text="Catatan :",font=font3),sticky="w")
 tkgrid(tklabel(guna,text="         Untuk mengerjakan Uji Normalitas klik menu UJI NORMALITAS lalu klik submenu SHAPIRO WILK.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="         Jika berdasarkan uji normalitas data tersebut merupakan data tidak normal, maka gunakan UJI MANN WHITNEY dan jika tidak, maka gunakan UJI TWO SAMPLE yang sudah tersedia pada menu.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="         Pada Uji Two Sample, terdapat 3 asumsi pengujian. Pilih asumsi yang sesuai dengan kasus yang akan diuji.",font=font2),sticky="w") 
 tkgrid(tklabel(guna,text="         Untuk mengerjakan data berpasangan klik menu PAIRED TEST.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="         Pada submenu pengujian, terdapat 3 pilihan pemasukan data, yaitu Data Ringkas, Data Input atau Data Import.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="         Jika anda menyimpan data anda pada SPSS, gunakan pilihan Import data.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="         Namun jika anda akan memasukkan data, pilih Input Data dan masukkan data secara manual ke dalam tabel yang telah disediakan.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text=" ")) 

 tkgrid(tklabel(guna,text="A. Cara mengerjakan dengan Ringkasan Data",font=font3),sticky="w")
 tkgrid(tklabel(guna,text="1. Pertama, pilih submenu Data Ringkasan. Kemudian akan muncul kotak isian hipotesis, nilai alfa dan banyak data.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="2. Masukkan hipotesis ke dalam kotak isian H0 dan H1, masukkan banyak data pertama (n1) dan banyak data kedua (n2) kedalam kotak isian dan geser slider untuk menentukan nilai alfa",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="3. Klik tombol 'MASUKKAN DATA' dan program akan menampilkan kotak isian rata - rata dan simpangan baku. Selanjutnya masukkan informasi data ringkas anda",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="4. Klik tombol 'SELESAI' dan program akan kembali ke jendela awal.",font=font2),sticky="w")  
 tkgrid(tklabel(guna,text="5. Klik tombol 'HASIL ANALISIS' dan akan muncul output berupa script yang berisikan hasil analisis, keputusan dan kesimpulan pengujian",font=font2),sticky="w")
 tkgrid(tklabel(guna,text=" "))  

 tkgrid(tklabel(guna,text="B. Cara mengerjakan dengan Input Data",font=font3),sticky="w")
 tkgrid(tklabel(guna,text="1. Pertama, pilih submenu Data Inputan. Kemudian akan muncul kotak isian hipotesis, nilai alfa dan banyak data.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="2. Masukkan hipotesis ke dalam kotak isian H0 dan H1, masukkan banyak data pertama (n1) dan banyak data kedua (n2) kedalam kotak isian dan geser slider untuk menentukan nilai alfa",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="3. Klik tombol 'MASUKKAN DATA' dan program akan menampilkan tabel isian pertama dan kedua. Selanjutnya masukkan data anda",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="4. Klik tombol 'SELESAI' dan program akan kembali ke jendela awal dan akan muncul tombol 'LIHAT DATA' untuk melihat data yang telat diinputkan.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="5. Klik tombol 'HASIL ANALISIS' dan akan muncul output berupa script yang berisikan hasil analisis, keputusan dan kesimpulan pengujian",font=font2),sticky="w")
 tkgrid(tklabel(guna,text=" "))  

 tkgrid(tklabel(guna,text="C. Cara mengerjakan dengan Import",font=font3),sticky="w")
 tkgrid(tklabel(guna,text="1. Pertama, pilih submenu Data Import. Kemudian tentukan hipotesis ke dalam kotak isian H0 dan H1 dan nilai alfa sesuai kebutuhan data Anda.",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="2. Kemudian klik 'IMPORT'. Untuk Program ini file data yang digunakan harus berformat dari SPSS (sav).",font=font2),sticky="w")
 tkgrid(tklabel(guna,text="3. Apabila data sudah terimport, akan muncul tombol 'HASIL ANALISIS'. Klik tombol tersebut untuk menampilkan hasil analisis, keputusan dan kesimpulan pengujian.",font=font2),sticky="w")
tkgrid(tklabel(guna,text=" "))  

tkgrid(tklabel(guna,text="NB: Tombol 'LIHAT DATA' berguna untuk mengecek kembali apakah data yang diinputkan sudah benar.",font=font2),sticky="w")
tkgrid(tklabel(guna,text=" ")) 
 
kb=tkbutton(guna,text="KEMBALI",command=function()tkdestroy(guna),bg="lightviolet")
 tkgrid(kb)
}
#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



#xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



menu1<-tkmenu(topmenu,tearoff=FALSE)
menu2<-tkmenu(topmenu,tearoff=FALSE)
menu3<-tkmenu(topmenu,tearoff=FALSE)
menu4<-tkmenu(topmenu,tearoff=FALSE)
menu5<-tkmenu(topmenu,tearoff=FALSE)

sm1<-tkmenu(submenu,tearoff=FALSE)
sm2<-tkmenu(submenu,tearoff=FALSE)
sm3<-tkmenu(submenu,tearoff=FALSE)

tkadd(menu1,"cascade",label="Variansi Diketahui",menu=sm1)
tkadd(menu1,"cascade",label="Asumsi Variansi Sama",menu=sm2)
tkadd(menu1,"cascade",label="Asumsi Variansi Tidak Sama",menu=sm3)

tkadd(sm1,"command",label="DATA RINGKASAN",command=function()asumsi1ringkasan())
tkadd(sm1,"command",label="DATA INPUT",command=function()asumsi1input())
tkadd(sm1,"command",label="DATA IMPORT SPSS",command=function()asumsi1import())

tkadd(sm2,"command",label="DATA RINGKASAN",command=function()asumsi2ringkasan())
tkadd(sm2,"command",label="DATA INPUT",command=function()asumsi2input())
tkadd(sm2,"command",label="DATA IMPORT SPSS",command=function()asumsi2import())

tkadd(sm3,"command",label="DATA RINGKASAN",command=function()asumsi3ringkasan())
tkadd(sm3,"command",label="DATA INPUT",command=function()asumsi3input())
tkadd(sm3,"command",label="DATA IMPORT SPSS",command=function()asumsi3import())

tkadd(menu2,"command",label="DATA RINGKASAN",command=function()asumsi4ringkasan())
tkadd(menu2,"command",label="DATA INPUT",command=function()asumsi4input())
tkadd(menu2,"command",label="DATA IMPORT SPSSS",command=function()asumsi4import())

tkadd(menu3,"command",label="DATA INPUT",command=function()mwinput())
tkadd(menu3,"command",label="DATA IMPORT SPSSS",command=function()mwimport())

tkadd(menu4,"command",label="DATA INPUT",command=function()normalinput())
tkadd(menu4,"command",label="DATA IMPORT SPSSS",command=function()normalimport())

tkadd(menu5,"command",label="DESKRIPSI ANALISIS UJI",command=function()deskripsi())
tkadd(menu5,"command",label="PANDUAN PENGGUNAAN PROGRAM",command=function()penggunaan())

tkadd(topmenu,"cascade",label="TWO SAMPLE",menu=menu1)
tkadd(topmenu,"cascade",label="PAIRED TEST",menu=menu2)
tkadd(topmenu,"cascade",label="MANN-WHITNEY TEST",menu=menu3)
tkadd(topmenu,"cascade",label="NORMALITY TEST",menu=menu4)
tkadd(topmenu,"cascade",label="INFO",menu=menu5)


