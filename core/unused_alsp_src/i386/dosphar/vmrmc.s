.xlisti
        extrn SMALL?:word
        extrn setrmivec:near
        include adefs.asm
        include mac1.asm
LODATA segment page4k er public use16 'LOW'
LOWDBASE        dd      $
                public  LOWDBASE
        dd      1000h dup(0)
pgfhstacktop    dd      $
        public pgfhstacktop
lowcodearea     dd      ?
        public lowcodearea                
guptas  db      'GTI SQL',0
        align   4                
guptaintnum     dd      0
        public  guptaintnum
guptafind_a     dd      guptafind-LOWDBASE
        public  guptafind_a        
rmpasson_a      dd      rmpasson-LOWDBASE
        public  rmpasson_a        
rmlinkdata      dd      5 dup(0)
        public  rmlinkdata
rmdosbase       dd      ?
        public  rmdosbase
guptaparms      dw      32 dup(?)
        public  guptaparms
        align   4
guptadatab      db      1000h dup(?)
        public  guptadatab
guptadatabs     dd      1000h        
        public  guptadatabs
vmiobuff        dd      1000h dup(?)
        public vmiobuff
tickcount       dd      0
oldint1c        dd      0
newint1c        dd      int1ch-LOWDBASE
        public tickcount,oldint1c,newint1c
int1ch: inc  dword ptr cs:[tickcount-LOWDBASE]
        jmp  dword ptr cs:[oldint1c -LOWDBASE]
        public  int1ch                   
__vmread  proc    far
        public  __vmread
        mov     bp,sp
        mov     ax,4200h
        mov     bx,[bp][4]
        mov     cx,[bp][10]
        mov     dx,[bp][8]
        int     21h
        jsc     nc,#a
        mov     ax,-1
        ret
#a:     mov     ax,3f00h
        mov     bx,[bp][4]
        mov     cx,[bp][6]
        mov     dx,vmiobuff-LOWDBASE
        int     21h
        jsc     nc,#b
        mov     ax,-2
#b:     ret
__vmread  endp                                                
;
__vmwrite  proc    far
        public  __vmwrite
        mov     bp,sp
        mov     ax,4200h
        mov     bx,[bp][4]
        mov     cx,[bp][10]
        mov     dx,[bp][8]
        int     21h
        jsc     nc,#a
        mov     ax,-1
        ret
#a:     mov     ax,4000h
        mov     bx,[bp][4]
        mov     cx,[bp][6]
        mov     dx,vmiobuff-LOWDBASE
        int     21h
        jsc     nc,#b
        mov     ax,-2
#b:     ret
__vmwrite  endp                                                
;
guptafind       proc    far
        cld
        mov     dword ptr ds:[guptaintnum-LOWDBASE],0
        mov     si,80h
        xor     ax,ax
        mov     fs,ax
#a:     cmp     si,400h
        jae     #r
        cmp     byte ptr fs:[si][3],0f0h
        jae     #b
        cmp     word ptr fs:[si][2],40h
        jna     #b
        les     di,dword ptr fs:[si]
        sub     di,8
        jc      #b
        push    si
        mov     si,guptas-LOWDBASE
        mov     cx,7
        repe    cmpsb
        pop     si
        jne     #b
        movzx   eax,si
        shr     eax,2
        mov     dword ptr ds:[guptaintnum-LOWDBASE],eax
        mov     di,#t-LOWDBASE
        mov     [di][1],al
        mov     ax,8001h
        push    ds
        mov     di,cs
        push    di
        call    #t
        pop     ds
        cmp     ax,66
        jne     #r
        ret
#b:     lea     si,[si][4]
        jmp     #a
#r:     xor     eax,eax
        mov     dword ptr ds:[guptaintnum-LOWDBASE],eax
        ret                                    
#t:     int     0        
        ret
guptafind       endp      
;
rmpasson        proc    far
        add     sp,4
        ret
rmpasson        endp          
;
LODATA ends
PMBASE segment page4k rw public use32 'CODE'
pmbase  dd      $
npginpfhs       dd      2
        public  npginpfhs        
        public  pmbase
guptafindaddr   dd      guptafind
        public guptafindaddr        
PMBASE ends   
        assume  ds:XDATA
        assume  cs:XCODE
XDATA segment dword rw public use32 'DATA'
apcseg  dw      ?
apdseg  dw      ?
apseglba        dd      ?
apsegso dd      ?
        public  apcseg,apdseg,apseglba,apsegso
oldmvsegh       label   fword
oldmvsegho      dd      ?
oldmvseghs      dw      ?
                dw      0      
XDATA ends   
XCODE segment dword er public use32 'CODE'
        extrn   seglim:near
        extrn   seglba:near
crash_  proc    near
        ntrn    crash
        movzx   eax,asb
        mov     ah,4ch
        int     21h
crash_  endp
        public  crash        
;
__vmbufops      proc    near
;
        ntrn    vmfread,<ebp,ebx,esi,edi>
        push    asd[4]
        mov     eax,asd[12]
        shl     eax,10h
        mov     ax,asw[4]
        push    eax
        mov     eax,250eh
        mov     ebx,dpt ds:rmdosbase
        add     ebx,__vmread-LOWDBASE
        mov     ecx,4
        int     21h
        jsc     nc,#a
        mov     eax,-3
#a:     movsx   eax,ax                        
        add     esp,8
        rets
;
        ntrn    vmfwrite,<ebp,ebx,esi,edi>
        push    asd[4]
        mov     eax,asd[12]
        shl     eax,10h
        mov     ax,asw[4]
        push    eax
        mov     eax,250eh
        mov     ebx,dpt ds:rmdosbase
        add     ebx,__vmwrite-LOWDBASE
        mov     ecx,4
        int     21h
        jsc     nc,#b
        mov     eax,-3
#b:     movsx   eax,ax                        
        add     esp,8
        rets
__vmbufops    endp
;
newmvsegh       proc    far
        push    eax
        movzx   eax,wpt[esp][12]
        sub     eax,0ch
        jsc     b,#x
        cmp     eax,7
        jsc     a,#x
        push    ds
        mov     ds,cs:apdseg
        mov     eax,[esp][20]
        mov     apseglba,eax
        pop     ds
#x:     pop     eax
        jmp     cs:oldmvsegh        
newmvsegh       endp
;
        ntrn    apsegsetup
        mov     apcseg,cs
        mov     apdseg,ds
        ccall   seglim,<ds>,4
        mov     apsegso,eax
        ccall   seglba,<ds>,4
        mov     apseglba,eax
        push    es
        mov     es,apcseg
        lea     ebx,newmvsegh
        mov     eax,2518h
        int     21h
        mov     oldmvsegho,ebx
        mov     oldmvseghs,es
        pop     es
        rets
;
_shutdown proc near
        ntrn    shutdown
        cmp     ds:oldint1c,0
        jsc     e,#a
        push    ds:oldint1c
        push    1ch
        call    setrmivec
        add     esp,8
#a:     xor     eax,eax
        rets                
_shutdown       endp        
XCODE ends
CGROUP  group  PMBASE,XCODE        
   end
