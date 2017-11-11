" Vim syntax file for WinAPI
" Language:     C/C++
" Maintainer:   Josh Seba <sebajosh@outlook.com>
" Last Change:  2017 Nov 7
"

syn keyword winapiConstant      ERROR_SUCCESS

syn keyword winapiType          VOID PVOID LPVOID LPCVOID
syn keyword winapiType          HANDLE LPHANDLE PHANDLE
syn keyword winapiType          POINTER_32 POINTER_64 POINTER_SIGNED POINTER_UNSIGNED
syn keyword winapiType          HINSTANCE HFILE HWND
syn keyword winapiType          BOOL BOOLEAN LPBOOL PBOOL PBOOLEAN
syn keyword winapiType          BYTE LBYTE TBYTE PBYTE PTBYTE
syn keyword winapiType          SHORT USHORT PSHORT PUSHORT
syn keyword winapiType          SIZE_T SSIZE_T PSIZE_T PSSIZE_T
syn keyword winapiType          WORD LPWORD PWORD
syn keyword winapiType          DWORD DWORDLONG DWORD32 DWORD64
syn keyword winapiType          LPDWORD PDWORD PDWORDLONG PDWORD32 PDWORD64
syn keyword winapiType          QWORD
syn keyword winapiType          INT INT8 INT16 INT32 INT64
syn keyword winapiType          LPINT LPLONG
syn keyword winapiType          PINT PINT8 PINT16 PINT32 PINT64
syn keyword winapiType          UINT UINT8 UINT16 UINT32 UINT64
syn keyword winapiType          PUINT PUINT8 PUINT16 PUINT32 PUINT64
syn keyword winapiType          LONG LONGLONG LONG32 LONG64
syn keyword winapiType          PLONG PLONGLONG PLONG32 PLONG64
syn keyword winapiType          ULONG ULONGLONG ULONG32 ULONG64
syn keyword winapiType          PULONG PULONGLONG PULONG32 PULONG64
syn keyword winapiType          FLOAT PFLOAT
syn keyword winapiType          HALF_PTR UHALF_PTR INT_PTR UINT_PTR LONG_PTR ULONG_PTR DWORD_PTR
syn keyword winapiType          PHALF_PTR PUHALF_PTR PINT_PTR PUINT_PTR PLONG_PTR PULONG_PTR PDWORD_PTR
syn keyword winapiType          CCHAR CHAR TCHAR UCHAR WCHAR
syn keyword winapiType          PCHAR PTCHAR PUCHAR PWCHAR
syn keyword winapiType          PSTR PTSTR PWSTR
syn keyword winapiType          LPSTR LPTSTR LPWSTR
syn keyword winapiType          LPCSTR LPCTSTR LPCWSTR PCSTR PCTSTR PCWSTR
syn keyword winapiType          HRESULT LPRESULT
syn keyword winapiType          GUID

syn keyword winapiMacro         WINAPI IN OUT OPTIONAL
syn keyword winapiMacro         BAIL_ON_ERROR BAIL_ON_HRESULT_ERROR BAIL_ON_APP_ERROR

syn keyword winapiStorageClass  CONST

syn keyword winapiAnnotation    __success
syn keyword winapiAnnotation    __reserved
syn keyword winapiAnnotation    _In_ _Inout_
syn keyword winapiAnnotation    __in __in_bcount __in_ecount __in_opt
syn keyword winapiAnnotation    _Out_ _Out_writes_ _Out_writes_to_
syn keyword winapiAnnotation    __out __out_ecount __out_ecount_z __out_opt
syn keyword winapiAnnotation    __inout __inout_opt
syn keyword winapiAnnotation    __deref_out __deref_out_bcount

" Define highlighting
hi def link winapiAnnotation      cOperator
hi def link winapiIdentifier      cIdentifier
hi def link winapiType            cType
hi def link winapiConstant        cConstant
hi def link winapiMacro           Macro
hi def link winapiStorageClass    cStorageClass
