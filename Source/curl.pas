unit curl;
{
curl.pas是curl.h和multi.h的Delphi接口定义，具体的使用方法及帮助请参考libcurl的
官方使用帮助。
代码转换：swish
邮箱：chinawsb@sina.com
版权协议：BSD
}
interface
uses windows,winsock;
const
  CURL_MAX_WRITE_SIZE=16384;
  CURL_MAX_HTTP_HEADER=102400;
  CURLFINFOFLAG_KNOWN_FILENAME=1;
  CURLFINFOFLAG_KNOWN_FILETYPE=2;
  CURLFINFOFLAG_KNOWN_TIME=4;
  CURLFINFOFLAG_KNOWN_PERM=8;
  CURLFINFOFLAG_KNOWN_UID=16;
  CURLFINFOFLAG_KNOWN_GID=32;
  CURLFINFOFLAG_KNOWN_SIZE=64;
  CURLFINFOFLAG_KNOWN_HLINKCOUNT=128;
  CURL_CHUNK_BGN_FUNC_OK=0;
  CURL_CHUNK_BGN_FUNC_FAIL=1;
  CURL_CHUNK_BGN_FUNC_SKIP=2;
  CURL_CHUNK_END_FUNC_OK=0;
  CURL_CHUNK_END_FUNC_FAIL=1;
  CURL_FNMATCHFUNC_MATCH=0;
  CURL_FNMATCHFUNC_NOMATCH=1;
  CURL_FNMATCHFUNC_FAIL=2;
///* These are the return codes for the seek callbacks */
  CURL_SEEKFUNC_OK=0;
  CURL_SEEKFUNC_FAIL=1;
  CURL_SEEKFUNC_CANTSEEK=2;
//  * This is a return code for the read callback that, when returned, will
//   signal libcurl to immediately abort the current transfer. */
  CURL_READFUNC_ABORT=$10000000;
///* This is a return code for the read callback that, when returned, will
//   signal libcurl to pause sending data on the current transfer. */
  CURL_READFUNC_PAUSE=$10000001;
//  /* The return code from the sockopt_callback can signal information back
//   to libcurl: */
  CURL_SOCKOPT_OK=0;
  CURL_SOCKOPT_ERROR=1;
  CURL_SOCKOPT_ALREADY_CONNECTED=2;

(*
 * Bitmasks for CURLOPT_HTTPAUTH and CURLOPT_PROXYAUTH options:
 *
 * CURLAUTH_NONE         - No HTTP authentication
 * CURLAUTH_BASIC        - HTTP Basic authentication (default)
 * CURLAUTH_DIGEST       - HTTP Digest authentication
 * CURLAUTH_GSSNEGOTIATE - HTTP GSS-Negotiate authentication
 * CURLAUTH_NTLM         - HTTP NTLM authentication
 * CURLAUTH_DIGEST_IE    - HTTP Digest authentication with IE flavour
 * CURLAUTH_NTLM_WB      - HTTP NTLM authentication delegated to winbind helper
 * CURLAUTH_ONLY         - Use together with a single other type to force no
 *                         authentication or just that single type
 * CURLAUTH_ANY          - All fine types set
 * CURLAUTH_ANYSAFE      - All fine types except Basic
 *)
  CURLAUTH_NONE=0;
  CURLAUTH_BASIC=1;
  CURLAUTH_DIGEST=2;
  CURLAUTH_GSSNEGOTIATE=4;
  CURLAUTH_NTLM=8;
  CURLAUTH_DIGEST_IE=16;
  CURLAUTH_NTLM_WB=32;
  CURLAUTH_ONLY=$80000000;
  CURLAUTH_ANY=$FFFFFFEF;//(~CURLAUTH_DIGEST_IE)
  CURLAUTH_ANYSAFE=$FFFFFFEE;//(~(CURLAUTH_BASIC|CURLAUTH_DIGEST_IE))
  CURLSSH_AUTH_ANY=$FFFFFFFF;
  CURLSSH_AUTH_NONE=0;
  CURLSSH_AUTH_PUBLICKEY=1;
  CURLSSH_AUTH_PASSWORD=2;
  CURLSSH_AUTH_HOST=4;
  CURLSSH_AUTH_KEYBOARD=8;
  CURLSSH_AUTH_AGENT=16;
  CURLSSH_AUTH_DEFAULT=CURLSSH_AUTH_ANY;
  CURLGSSAPI_DELEGATION_NONE=0;
  CURLGSSAPI_DELEGATION_POLICY_FLAG=1;
  CURLGSSAPI_DELEGATION_FLAG=2;
  CURL_ERROR_SIZE=256;
  (*/* Definition of bits for the CURLOPT_SSL_OPTIONS argument: */

/* - ALLOW_BEAST tells libcurl to allow the BEAST SSL vulnerability in the
   name of improving interoperability with older servers. Some SSL libraries
   have introduced work-arounds for this flaw but those work-arounds sometimes
   make the SSL communication fail. To regain functionality with those broken
   servers, a user can this way allow the vulnerability back. */*)
  CURLSSLOPT_ALLOW_BEAST=1;
(*
/* CURLPROTO_ defines are for the CURLOPT_*PROTOCOLS options */
#define CURLPROTO_HTTP   (1<<0)
#define CURLPROTO_HTTPS  (1<<1)
#define CURLPROTO_FTP    (1<<2)
#define CURLPROTO_FTPS   (1<<3)
#define CURLPROTO_SCP    (1<<4)
#define CURLPROTO_SFTP   (1<<5)
#define CURLPROTO_TELNET (1<<6)
#define CURLPROTO_LDAP   (1<<7)
#define CURLPROTO_LDAPS  (1<<8)
#define CURLPROTO_DICT   (1<<9)
#define CURLPROTO_FILE   (1<<10)
#define CURLPROTO_TFTP   (1<<11)
#define CURLPROTO_IMAP   (1<<12)
#define CURLPROTO_IMAPS  (1<<13)
#define CURLPROTO_POP3   (1<<14)
#define CURLPROTO_POP3S  (1<<15)
#define CURLPROTO_SMTP   (1<<16)
#define CURLPROTO_SMTPS  (1<<17)
#define CURLPROTO_RTSP   (1<<18)
#define CURLPROTO_RTMP   (1<<19)
#define CURLPROTO_RTMPT  (1<<20)
#define CURLPROTO_RTMPE  (1<<21)
#define CURLPROTO_RTMPTE (1<<22)
#define CURLPROTO_RTMPS  (1<<23)
#define CURLPROTO_RTMPTS (1<<24)
#define CURLPROTO_GOPHER (1<<25)
#define CURLPROTO_ALL    (~0) /* enable everything */
*)
  CURLPROTO_HTTP=(1 shl 0);
  CURLPROTO_HTTPS=(1 shl 1);
  CURLPROTO_FTP=(1 shl 2);
  CURLPROTO_FTPS=(1 shl 3);
  CURLPROTO_SCP=(1 shl 4);
  CURLPROTO_SFTP=(1 shl 5);
  CURLPROTO_TELNET=(1 shl 6);
  CURLPROTO_LDAP=(1 shl 7);
  CURLPROTO_LDAPS=(1 shl 8);
  CURLPROTO_DICT=(1 shl 9);
  CURLPROTO_FILE=(1 shl 10);
  CURLPROTO_TFTP=(1 shl 11);
  CURLPROTO_IMAP=(1 shl 12);
  CURLPROTO_IMAPS=(1 shl 13);
  CURLPROTO_POP3=(1 shl 14);
  CURLPROTO_POP3S=(1 shl 15);
  CURLPROTO_SMTP=(1 shl 16);
  CURLPROTO_SMTPS=(1 shl 17);
  CURLPROTO_RTSP=(1 shl 18);
  CURLPROTO_RTMP=(1 shl 19);
  CURLPROTO_RTMPT=(1 shl 20);
  CURLPROTO_RTMPE=(1 shl 21);
  CURLPROTO_RTMPTE=(1 shl 22);
  CURLPROTO_RTMPS=(1 shl 23);
  CURLPROTO_RTMPTS=(1 shl 24);
  CURLPROTO_GOPHER=(1 shl 25);
  CURLPROTO_ALL=$FFFFFFFF;
(*
#define CURLOPTTYPE_LONG          0
#define CURLOPTTYPE_OBJECTPOINT   10000
#define CURLOPTTYPE_FUNCTIONPOINT 20000
#define CURLOPTTYPE_OFF_T         30000
*)
  CURLOPTTYPE_LONG=0;
  CURLOPTTYPE_OBJECTPOINT=10000;
  CURLOPTTYPE_FUNCTIONPOINT=20000;
  CURLOPTTYPE_OFF_T=30000;
(*/* Below here follows defines for the CURLOPT_IPRESOLVE option. If a host
     name resolves addresses using more than one IP protocol version, this
     option might be handy to force libcurl to use a specific IP version. */
#define CURL_IPRESOLVE_WHATEVER 0 /* default, resolves addresses to all IP
                                     versions that your system allows */
#define CURL_IPRESOLVE_V4       1 /* resolve to ipv4 addresses */
#define CURL_IPRESOLVE_V6       2 /* resolve to ipv6 addresses */
*)
  CURL_IPRESOLVE_WHATEVER=0;
  CURL_IPRESOLVE_V4=1;
  CURL_IPRESOLVE_V6=2;

(*
/* symbols to use with CURLOPT_POSTREDIR.
   CURL_REDIR_POST_301, CURL_REDIR_POST_302 and CURL_REDIR_POST_303
   can be bitwise ORed so that CURL_REDIR_POST_301 | CURL_REDIR_POST_302
   | CURL_REDIR_POST_303 == CURL_REDIR_POST_ALL */

#define CURL_REDIR_GET_ALL  0
#define CURL_REDIR_POST_301 1
#define CURL_REDIR_POST_302 2
#define CURL_REDIR_POST_303 4
#define CURL_REDIR_POST_ALL \
    (CURL_REDIR_POST_301|CURL_REDIR_POST_302|CURL_REDIR_POST_303)
*)
  CURL_REDIR_GET_ALL=0;
  CURL_REDIR_POST_301=1;
  CURL_REDIR_POST_302=2;
  CURL_REDIR_POST_303=4;
  CURL_REDIR_POST_ALL=7;
(*
#define CURLINFO_STRING   0x100000
#define CURLINFO_LONG     0x200000
#define CURLINFO_DOUBLE   0x300000
#define CURLINFO_SLIST    0x400000
#define CURLINFO_MASK     0x0fffff
#define CURLINFO_TYPEMASK 0xf00000
*)
  CURLINFO_STRING=$100000;
  CURLINFO_LONG=$200000;
  CURLINFO_DOUBLE=$300000;
  CURLINFO_SLIST=$400000;
  CURLINFO_MASK=$0fffff;
  CURLINFO_TYPEMASK=$f00000;

  CURL_GLOBAL_SSL=(1 shl 0);
  CURL_GLOBAL_WIN32=(1 shl 1);
  CURL_GLOBAL_ALL=(CURL_GLOBAL_SSL or CURL_GLOBAL_WIN32);
  CURL_GLOBAL_NOTHING=0;
  CURL_GLOBAL_DEFAULT=CURL_GLOBAL_ALL;
  CURL_GLOBAL_ACK_EINTR=(1 shl 2);
(*
#define CURL_VERSION_IPV6      (1<<0)  /* IPv6-enabled */
#define CURL_VERSION_KERBEROS4 (1<<1)  /* kerberos auth is supported */
#define CURL_VERSION_SSL       (1<<2)  /* SSL options are present */
#define CURL_VERSION_LIBZ      (1<<3)  /* libz features are present */
#define CURL_VERSION_NTLM      (1<<4)  /* NTLM auth is supported */
#define CURL_VERSION_GSSNEGOTIATE (1<<5) /* Negotiate auth support */
#define CURL_VERSION_DEBUG     (1<<6)  /* built with debug capabilities */
#define CURL_VERSION_ASYNCHDNS (1<<7)  /* asynchronous dns resolves */
#define CURL_VERSION_SPNEGO    (1<<8)  /* SPNEGO auth */
#define CURL_VERSION_LARGEFILE (1<<9)  /* supports files bigger than 2GB */
#define CURL_VERSION_IDN       (1<<10) /* International Domain Names support */
#define CURL_VERSION_SSPI      (1<<11) /* SSPI is supported */
#define CURL_VERSION_CONV      (1<<12) /* character conversions supported */
#define CURL_VERSION_CURLDEBUG (1<<13) /* debug memory tracking supported */
#define CURL_VERSION_TLSAUTH_SRP (1<<14) /* TLS-SRP auth is supported */
#define CURL_VERSION_NTLM_WB   (1<<15) /* NTLM delegating to winbind helper */
*)
  CURL_VERSION_IPV6=(1 shl 0);
  CURL_VERSION_KERBEROS4=(1 shl 1);
  CURL_VERSION_SSL=(1 shl 2);
  CURL_VERSION_LIBZ=(1 shl 3);
  CURL_VERSION_NTLM=(1 shl 4);
  CURL_VERSION_GSSNEGOTIATE=(1 shl 5);
  CURL_VERSION_DEBUG=(1 shl 6);
  CURL_VERSION_ASYNCHDNS=(1 shl 7);
  CURL_VERSION_SPNEGO=(1 shl 8);
  CURL_VERSION_LARGEFILE=(1 shl 9);
  CURL_VERSION_IDN=(1 shl 10);
  CURL_VERSION_SSPI=(1 shl 11);
  CURL_VERSION_CONV=(1 shl 12);
  CURL_VERSION_CURLDEBUG=(1 shl 13);
  CURL_VERSION_TLSAUTH_SRP=(1 shl 14);
  CURL_VERSION_NTLM_WB=(1 shl 15);
(*
#define CURLPAUSE_RECV      (1<<0)
#define CURLPAUSE_RECV_CONT (0)

#define CURLPAUSE_SEND      (1<<2)
#define CURLPAUSE_SEND_CONT (0)

#define CURLPAUSE_ALL       (CURLPAUSE_RECV|CURLPAUSE_SEND)
#define CURLPAUSE_CONT      (CURLPAUSE_RECV_CONT|CURLPAUSE_SEND_CONT)

*)
  CURLPAUSE_RECV=(1 shl 0);
  CURLPAUSE_RECV_CONT=(0);
  CURLPAUSE_SEND=(1 shl 2);
  CURLPAUSE_SEND_CONT=(0);
  CURLPAUSE_ALL=(CURLPAUSE_RECV or CURLPAUSE_SEND);
  CURLPAUSE_CONT=(CURLPAUSE_RECV_CONT or CURLPAUSE_SEND_CONT);
//multi.h
  CURL_POLL_NONE=0;
  CURL_POLL_IN=1;
  CURL_POLL_OUT=2;
  CURL_POLL_INOUT=3;
  CURL_POLL_REMOVE=4;
  CURL_SOCKET_TIMEOUT=INVALID_SOCKET;
  CURL_CSELECT_IN=$01;
  CURL_CSELECT_OUT=$02;
  CURL_CSELECT_ERR=$04;
(*typedef enum {
  CURL_FORMADD_OK, /* first, no error */

  CURL_FORMADD_MEMORY,
  CURL_FORMADD_OPTION_TWICE,
  CURL_FORMADD_NULL,
  CURL_FORMADD_UNKNOWN_OPTION,
  CURL_FORMADD_INCOMPLETE,
  CURL_FORMADD_ILLEGAL_ARRAY,
  CURL_FORMADD_DISABLED, /* libcurl was built with this disabled */

  CURL_FORMADD_LAST /* last */
} CURLFORMcode;*)
type
  (*
/* use this for multipart formpost building */
/* Returns code for curl_formadd()
 *
 * Returns:
 * CURL_FORMADD_OK             on success
 * CURL_FORMADD_MEMORY         if the FormInfo allocation fails
 * CURL_FORMADD_OPTION_TWICE   if one option is given twice for one Form
 * CURL_FORMADD_NULL           if a null pointer was given for a char
 * CURL_FORMADD_MEMORY         if the allocation of a FormInfo struct failed
 * CURL_FORMADD_UNKNOWN_OPTION if an unknown option was used
 * CURL_FORMADD_INCOMPLETE     if the some FormInfo is not complete (or error)
 * CURL_FORMADD_MEMORY         if a curl_httppost struct cannot be allocated
 * CURL_FORMADD_MEMORY         if some allocation for string copying failed.
 * CURL_FORMADD_ILLEGAL_ARRAY  if an illegal option is used in an array
 *
 ***************************************************************************/
typedef enum {
  CURL_FORMADD_OK, /* first, no error */

  CURL_FORMADD_MEMORY,
  CURL_FORMADD_OPTION_TWICE,
  CURL_FORMADD_NULL,
  CURL_FORMADD_UNKNOWN_OPTION,
  CURL_FORMADD_INCOMPLETE,
  CURL_FORMADD_ILLEGAL_ARRAY,
  CURL_FORMADD_DISABLED, /* libcurl was built with this disabled */

  CURL_FORMADD_LAST /* last */
} CURLFORMcode;

*)


  TCUrlFormCode=(
    CURL_FORMADD_OK,
    CURL_FORMADD_MEMORY,
    CURL_FORMADD_OPTION_TWICE,
    CURL_FORMADD_NULL,
    CURL_FORMADD_UNKNOWN_OPTION,
    CURL_FORMADD_INCOMPLETE,
    CURL_FORMADD_ILLEGAL_ARRAY,
    CURL_FORMADD_DISABLED, // libcurl was built with this disabled
    CURL_FORMADD_LAST
    );
(*

struct curl_httppost {
  struct curl_httppost *next;       /* next entry in the list */
  char *name;                       /* pointer to allocated name */
  long namelength;                  /* length of name length */
  char *contents;                   /* pointer to allocated data contents */
  long contentslength;              /* length of contents field */
  char *buffer;                     /* pointer to allocated buffer contents */
  long bufferlength;                /* length of buffer field */
  char *contenttype;                /* Content-Type */
  struct curl_slist* contentheader; /* list of extra headers for this form */
  struct curl_httppost *more;       /* if one field name has more than one
                                       file, this link should link to following
                                       files */
  long flags;                       /* as defined below */
#define HTTPPOST_FILENAME (1<<0)    /* specified content is a file name */
#define HTTPPOST_READFILE (1<<1)    /* specified content is a file name */
#define HTTPPOST_PTRNAME (1<<2)     /* name is only stored pointer
                                       do not free in formfree */
#define HTTPPOST_PTRCONTENTS (1<<3) /* contents is only stored pointer
                                       do not free in formfree */
#define HTTPPOST_BUFFER (1<<4)      /* upload file from buffer */
#define HTTPPOST_PTRBUFFER (1<<5)   /* upload file from pointer contents */
#define HTTPPOST_CALLBACK (1<<6)    /* upload file contents by using the
                                       regular read callback to get the data
                                       and pass the given pointer as custom
                                       pointer */

  char *showfilename;               /* The file name to show. If not set, the
                                       actual file name will be used (if this
                                       is a file part) */
  void *userp;                      /* custom pointer used for
                                       HTTPPOST_CALLBACK posts */
};
*)

(*
  /* linked-list structure for the CURLOPT_QUOTE option (and other) */
struct curl_slist {
  char *data;
  struct curl_slist *next;
};

*)
  PCurlSList=^TCurlSList;
  TCurlSList=record
    data:PAnsiChar;
    next:PCurlSList;
  end;
  PCurlHttpPost=^TCUrlHttpPost;
  TCUrlHttpPost=record
    Next:PCurlHttpPost;
    name:PAnsiChar;
    namelength:Integer;
    contents:PAnsiChar;
    contentslength:Integer;
    buffer:PAnsiChar;
    bufferlength:Integer;
    contenttype:PAnsiChar;
    contentheader:PCurlSList;
    more:PCurlHttpPost;
    flags:Integer;
    showfilename:PAnsiChar;
    userp:Pointer;
  end;
  PPCurlHttpPost=^PCUrlHttpPost;
(*  typedef int (*curl_progress_callback)(void *clientp,
                                      double dltotal,
                                      double dlnow,
                                      double ultotal,
                                      double ulnow);*)
  TCurlProgressCallback=function(clientp:Pointer;dltotal,dlnow,ultotal,ulnow:Double):Integer;cdecl;
(*typedef size_t (*curl_write_callback)(char *buffer,
                                      size_t size,
                                      size_t nitems,
                                      void *outstream);*)
  TCurlWriteCallback=function(buffer:PAnsiChar;Size,nItems:Integer;outstream:Pointer):Integer;cdecl;
  
(*  /* enumeration of file types */
typedef enum {
  CURLFILETYPE_FILE = 0,
  CURLFILETYPE_DIRECTORY,
  CURLFILETYPE_SYMLINK,
  CURLFILETYPE_DEVICE_BLOCK,
  CURLFILETYPE_DEVICE_CHAR,
  CURLFILETYPE_NAMEDPIPE,
  CURLFILETYPE_SOCKET,
  CURLFILETYPE_DOOR, /* is possible only on Sun Solaris now */

  CURLFILETYPE_UNKNOWN /* should never occur */
} curlfiletype;
*)
  TCUrlFileType=(
  CURLFILETYPE_FILE = 0,
  CURLFILETYPE_DIRECTORY,
  CURLFILETYPE_SYMLINK,
  CURLFILETYPE_DEVICE_BLOCK,
  CURLFILETYPE_DEVICE_CHAR,
  CURLFILETYPE_NAMEDPIPE,
  CURLFILETYPE_SOCKET,
  CURLFILETYPE_DOOR,
  CURLFILETYPE_UNKNOWN
  );

(*/* Content of this structure depends on information which is known and is
   achievable (e.g. by FTP LIST parsing). Please see the url_easy_setopt(3) man
   page for callbacks returning this structure -- some fields are mandatory,
   some others are optional. The FLAG field has special meaning. */
struct curl_fileinfo {
  char *filename;
  curlfiletype filetype;
  time_t time;
  unsigned int perm;
  int uid;
  int gid;
  curl_off_t size;
  long int hardlinks;

  struct {
    /* If some of these fields is not NULL, it is a pointer to b_data. */
    char *time;
    char *perm;
    char *user;
    char *group;
    char *target; /* pointer to the target filename of a symlink */
  } strings;

  unsigned int flags;

  /* used internally */
  char * b_data;
  size_t b_size;
  size_t b_used;
};*)
  TCurlStrings=record
    time:PAnsiChar;
    perm:PAnsiChar;
    user:PAnsiChar;
    group:PAnsiChar;
    target:PAnsiChar;
  end;
  TCurlFileInfo=record
    filename:PAnsiChar;
    filetype:TCUrlFileType;
    time:Integer;
    perm:Cardinal;
    uid:Integer;
    gid:Integer;
    size:Int64;
    hardlinks:Integer;
    strings:TCurlStrings;
    flags:Cardinal;
    b_data:PAnsiChar;
    b_size:Integer;
    b_used:Integer;
  end;
(*/* If splitting of data transfer is enabled this callback is called after
   download of an individual chunk finished.
   Note! After this callback was set then it have to be called FOR ALL chunks.
   Even if downloading of this chunk was skipped in CHUNK_BGN_FUNC.
   This is the reason why we don't need "transfer_info" parameter in this
   callback and we are not interested in "remains" parameter too. */
typedef long (*curl_chunk_end_callback)(void *ptr);*)
  TCurlChunkEndCallback=function(ptr:Pointer):Integer;cdecl;
(*  /* callback type for wildcard downloading pattern matching. If the
   string matches the pattern, return CURL_FNMATCHFUNC_MATCH value, etc. */
typedef int (*curl_fnmatch_callback)(void *ptr,
                                     const char *pattern,
                                     const char *string);*)
  TCurlFnMatchCallback=function(ptr:Pointer;const pattern,str:PAnsiChar):Integer;cdecl;
(*typedef int (*curl_seek_callback)(void *instream,
                                  curl_off_t offset,
                                  int origin); /* 'whence' */*)
  TCurlSeekCallback=function(instream:Pointer;offset:Int64;origin:Integer):Integer;cdecl;

(*  typedef size_t (*curl_read_callback)(char *buffer,
                                      size_t size,
                                      size_t nitems,
                                      void *instream);
*)
  TCurlReadCallback=function(buffer:PAnsiChar;size,nitem:Integer;instream:Pointer):Integer;cdecl;
(*typedef enum  {
  CURLSOCKTYPE_IPCXN,  /* socket created for a specific IP connection */
  CURLSOCKTYPE_ACCEPT, /* socket created by accept() call */
  CURLSOCKTYPE_LAST    /* never use */
} curlsocktype;*)
  TCurlSockType=(
    CURLSOCKTYPE_IPCXN,  //socket created for a specific IP connection
    CURLSOCKTYPE_ACCEPT, //socket created by accept() call
    CURLSOCKTYPE_LAST
    );
(*  typedef int (*curl_sockopt_callback)(void *clientp,
                                     curl_socket_t curlfd,
                                     curlsocktype purpose);
*)
  TCurlSockOptCallback=function(clientp:Pointer;curlfd:TSocket;purpose:TCurlSockType):Integer;cdecl;
(*struct curl_sockaddr {
  int family;
  int socktype;
  int protocol;
  unsigned int addrlen; /* addrlen was a socklen_t type before 7.18.0 but it
                           turned really ugly and painful on the systems that
                           lack this type */
  struct sockaddr addr;
};*)
  TCurlSockAddr=record
    family:Integer;
    socktype:Integer;
    protocol:Integer;
    addrlen:Cardinal;
    addr:TSockAddr;
  end;
(*  typedef curl_socket_t
(*curl_opensocket_callback)(void *clientp,
                            curlsocktype purpose,
                            struct curl_sockaddr *address);
*)
  TCurlOpenSocketCallback=function (clientp:Pointer;purpose:TCUrlSockType;var address:TCUrlSockAddr):TSocket;cdecl;
(*  typedef int (*curl_closesocket_callback)(void *clientp, curl_socket_t item);
*)
  TCUrlCloseSocketCallback=function(clientp:Pointer;item:TSocket):Integer;
(*  typedef enum {
  CURLIOE_OK,            /* I/O operation successful */
  CURLIOE_UNKNOWNCMD,    /* command was unknown to callback */
  CURLIOE_FAILRESTART,   /* failed to restart the read */
  CURLIOE_LAST           /* never use */
} curlioerr;
*)
  TCUrlIOError=(
    CURLIOE_OK,            // I/O operation successful
    CURLIOE_UNKNOWNCMD,    // command was unknown to callback
    CURLIOE_FAILRESTART,   // failed to restart the read
    CURLIOE_LAST           // never use
    );
(*  typedef enum  {
  CURLIOCMD_NOP,         /* no operation */
  CURLIOCMD_RESTARTREAD, /* restart the read stream from start */
  CURLIOCMD_LAST         /* never use */
} curliocmd;
*)
  TCUrlIOCmd=(
    CURLIOCMD_NOP,         //no operation
    CURLIOCMD_RESTARTREAD, //restart the read stream from start
    CURLIOCMD_LAST         //never use
    );
(*
typedef curlioerr (*curl_ioctl_callback)(CURL *handle,
                                         int cmd,
                                         void *clientp);
*)
  TCurlIOCtlCallback=function(AHandle:THandle;cmd:Integer;clientp:Pointer):TCurlIOError;
(*
/*
 * The following typedef's are signatures of malloc, free, realloc, strdup and
 * calloc respectively.  Function pointers of these types can be passed to the
 * curl_global_init_mem() function to set user defined memory management
 * callback routines.
 */
typedef void *(*curl_malloc_callback)(size_t size);
typedef void (*curl_free_callback)(void *ptr);
typedef void *(*curl_realloc_callback)(void *ptr, size_t size);
typedef char *(*curl_strdup_callback)(const char *str);
typedef void *(*curl_calloc_callback)(size_t nmemb, size_t size);
*)
  TCurlMallocCallback=function (Size:Integer):Pointer;cdecl;
  TCurlFreeCallback=procedure(ptr:Pointer);cdecl;
  TCurlReallocCallback=function(ptr:Pointer;size:Integer):Pointer;cdecl;
  TCurlStrdupCallback=function(const str:PAnsiChar):PAnsiChar;cdecl;
  TCurlCallocCallback=function(nmemb,size:Integer):Pointer;
(*
/* the kind of data that is passed to information_callback*/
typedef enum {
  CURLINFO_TEXT = 0,
  CURLINFO_HEADER_IN,    /* 1 */
  CURLINFO_HEADER_OUT,   /* 2 */
  CURLINFO_DATA_IN,      /* 3 */
  CURLINFO_DATA_OUT,     /* 4 */
  CURLINFO_SSL_DATA_IN,  /* 5 */
  CURLINFO_SSL_DATA_OUT, /* 6 */
  CURLINFO_END
} curl_infotype;
*)
  TCUrlInfoType=(
    CURLINFO_TEXT = 0,
    CURLINFO_HEADER_IN,
    CURLINFO_HEADER_OUT,
    CURLINFO_DATA_IN,
    CURLINFO_DATA_OUT,
    CURLINFO_SSL_DATA_IN,
    CURLINFO_SSL_DATA_OUT,
    CURLINFO_END
  );
(*
typedef int (*curl_debug_callback)
       (CURL *handle,      /* the handle/transfer this concerns */
        curl_infotype type, /* what kind of data */
        char *data,        /* points to the data */
        size_t size,       /* size of the data pointed to */
        void *userptr);    /* whatever the user please */
*)
  TCurlDebugCallback=function(handle:THandle;atype:TCUrlInfoType;data:PAnsiChar;Size:Integer;userptr:Pointer):Integer;
  (*
  typedef enum {
  CURLE_OK = 0,
  CURLE_UNSUPPORTED_PROTOCOL,    /* 1 */
  CURLE_FAILED_INIT,             /* 2 */
  CURLE_URL_MALFORMAT,           /* 3 */
  CURLE_NOT_BUILT_IN,            /* 4 - [was obsoleted in August 2007 for
                                    7.17.0, reused in April 2011 for 7.21.5] */
  CURLE_COULDNT_RESOLVE_PROXY,   /* 5 */
  CURLE_COULDNT_RESOLVE_HOST,    /* 6 */
  CURLE_COULDNT_CONNECT,         /* 7 */
  CURLE_FTP_WEIRD_SERVER_REPLY,  /* 8 */
  CURLE_REMOTE_ACCESS_DENIED,    /* 9 a service was denied by the server
                                    due to lack of access - when login fails
                                    this is not returned. */
  CURLE_FTP_ACCEPT_FAILED,       /* 10 - [was obsoleted in April 2006 for
                                    7.15.4, reused in Dec 2011 for 7.24.0]*/
  CURLE_FTP_WEIRD_PASS_REPLY,    /* 11 */
  CURLE_FTP_ACCEPT_TIMEOUT,      /* 12 - timeout occurred accepting server
                                    [was obsoleted in August 2007 for 7.17.0,
                                    reused in Dec 2011 for 7.24.0]*/
  CURLE_FTP_WEIRD_PASV_REPLY,    /* 13 */
  CURLE_FTP_WEIRD_227_FORMAT,    /* 14 */
  CURLE_FTP_CANT_GET_HOST,       /* 15 */
  CURLE_OBSOLETE16,              /* 16 - NOT USED */
  CURLE_FTP_COULDNT_SET_TYPE,    /* 17 */
  CURLE_PARTIAL_FILE,            /* 18 */
  CURLE_FTP_COULDNT_RETR_FILE,   /* 19 */
  CURLE_OBSOLETE20,              /* 20 - NOT USED */
  CURLE_QUOTE_ERROR,             /* 21 - quote command failure */
  CURLE_HTTP_RETURNED_ERROR,     /* 22 */
  CURLE_WRITE_ERROR,             /* 23 */
  CURLE_OBSOLETE24,              /* 24 - NOT USED */
  CURLE_UPLOAD_FAILED,           /* 25 - failed upload "command" */
  CURLE_READ_ERROR,              /* 26 - couldn't open/read from file */
  CURLE_OUT_OF_MEMORY,           /* 27 */
  /* Note: CURLE_OUT_OF_MEMORY may sometimes indicate a conversion error
           instead of a memory allocation error if CURL_DOES_CONVERSIONS
           is defined
  */
  CURLE_OPERATION_TIMEDOUT,      /* 28 - the timeout time was reached */
  CURLE_OBSOLETE29,              /* 29 - NOT USED */
  CURLE_FTP_PORT_FAILED,         /* 30 - FTP PORT operation failed */
  CURLE_FTP_COULDNT_USE_REST,    /* 31 - the REST command failed */
  CURLE_OBSOLETE32,              /* 32 - NOT USED */
  CURLE_RANGE_ERROR,             /* 33 - RANGE "command" didn't work */
  CURLE_HTTP_POST_ERROR,         /* 34 */
  CURLE_SSL_CONNECT_ERROR,       /* 35 - wrong when connecting with SSL */
  CURLE_BAD_DOWNLOAD_RESUME,     /* 36 - couldn't resume download */
  CURLE_FILE_COULDNT_READ_FILE,  /* 37 */
  CURLE_LDAP_CANNOT_BIND,        /* 38 */
  CURLE_LDAP_SEARCH_FAILED,      /* 39 */
  CURLE_OBSOLETE40,              /* 40 - NOT USED */
  CURLE_FUNCTION_NOT_FOUND,      /* 41 */
  CURLE_ABORTED_BY_CALLBACK,     /* 42 */
  CURLE_BAD_FUNCTION_ARGUMENT,   /* 43 */
  CURLE_OBSOLETE44,              /* 44 - NOT USED */
  CURLE_INTERFACE_FAILED,        /* 45 - CURLOPT_INTERFACE failed */
  CURLE_OBSOLETE46,              /* 46 - NOT USED */
  CURLE_TOO_MANY_REDIRECTS ,     /* 47 - catch endless re-direct loops */
  CURLE_UNKNOWN_OPTION,          /* 48 - User specified an unknown option */
  CURLE_TELNET_OPTION_SYNTAX ,   /* 49 - Malformed telnet option */
  CURLE_OBSOLETE50,              /* 50 - NOT USED */
  CURLE_PEER_FAILED_VERIFICATION, /* 51 - peer's certificate or fingerprint
                                     wasn't verified fine */
  CURLE_GOT_NOTHING,             /* 52 - when this is a specific error */
  CURLE_SSL_ENGINE_NOTFOUND,     /* 53 - SSL crypto engine not found */
  CURLE_SSL_ENGINE_SETFAILED,    /* 54 - can not set SSL crypto engine as
                                    default */
  CURLE_SEND_ERROR,              /* 55 - failed sending network data */
  CURLE_RECV_ERROR,              /* 56 - failure in receiving network data */
  CURLE_OBSOLETE57,              /* 57 - NOT IN USE */
  CURLE_SSL_CERTPROBLEM,         /* 58 - problem with the local certificate */
  CURLE_SSL_CIPHER,              /* 59 - couldn't use specified cipher */
  CURLE_SSL_CACERT,              /* 60 - problem with the CA cert (path?) */
  CURLE_BAD_CONTENT_ENCODING,    /* 61 - Unrecognized/bad encoding */
  CURLE_LDAP_INVALID_URL,        /* 62 - Invalid LDAP URL */
  CURLE_FILESIZE_EXCEEDED,       /* 63 - Maximum file size exceeded */
  CURLE_USE_SSL_FAILED,          /* 64 - Requested FTP SSL level failed */
  CURLE_SEND_FAIL_REWIND,        /* 65 - Sending the data requires a rewind
                                    that failed */
  CURLE_SSL_ENGINE_INITFAILED,   /* 66 - failed to initialise ENGINE */
  CURLE_LOGIN_DENIED,            /* 67 - user, password or similar was not
                                    accepted and we failed to login */
  CURLE_TFTP_NOTFOUND,           /* 68 - file not found on server */
  CURLE_TFTP_PERM,               /* 69 - permission problem on server */
  CURLE_REMOTE_DISK_FULL,        /* 70 - out of disk space on server */
  CURLE_TFTP_ILLEGAL,            /* 71 - Illegal TFTP operation */
  CURLE_TFTP_UNKNOWNID,          /* 72 - Unknown transfer ID */
  CURLE_REMOTE_FILE_EXISTS,      /* 73 - File already exists */
  CURLE_TFTP_NOSUCHUSER,         /* 74 - No such user */
  CURLE_CONV_FAILED,             /* 75 - conversion failed */
  CURLE_CONV_REQD,               /* 76 - caller must register conversion
                                    callbacks using curl_easy_setopt options
                                    CURLOPT_CONV_FROM_NETWORK_FUNCTION,
                                    CURLOPT_CONV_TO_NETWORK_FUNCTION, and
                                    CURLOPT_CONV_FROM_UTF8_FUNCTION */
  CURLE_SSL_CACERT_BADFILE,      /* 77 - could not load CACERT file, missing
                                    or wrong format */
  CURLE_REMOTE_FILE_NOT_FOUND,   /* 78 - remote file not found */
  CURLE_SSH,                     /* 79 - error from the SSH layer, somewhat
                                    generic so the error message will be of
                                    interest when this has happened */

  CURLE_SSL_SHUTDOWN_FAILED,     /* 80 - Failed to shut down the SSL
                                    connection */
  CURLE_AGAIN,                   /* 81 - socket is not ready for send/recv,
                                    wait till it's ready and try again (Added
                                    in 7.18.2) */
  CURLE_SSL_CRL_BADFILE,         /* 82 - could not load CRL file, missing or
                                    wrong format (Added in 7.19.0) */
  CURLE_SSL_ISSUER_ERROR,        /* 83 - Issuer check failed.  (Added in
                                    7.19.0) */
  CURLE_FTP_PRET_FAILED,         /* 84 - a PRET command failed */
  CURLE_RTSP_CSEQ_ERROR,         /* 85 - mismatch of RTSP CSeq numbers */
  CURLE_RTSP_SESSION_ERROR,      /* 86 - mismatch of RTSP Session Ids */
  CURLE_FTP_BAD_FILE_LIST,       /* 87 - unable to parse FTP file list */
  CURLE_CHUNK_FAILED,            /* 88 - chunk callback reported error */
  CURLE_NO_CONNECTION_AVAILABLE, /* 89 - No connection available, the
                                    session will be queued */
  CURL_LAST /* never use! */
} CURLcode;
  *)
    TCUrlCode=(
      CURLE_OK = 0,
      CURLE_UNSUPPORTED_PROTOCOL,
      CURLE_FAILED_INIT,
      CURLE_URL_MALFORMAT,
      CURLE_NOT_BUILT_IN,
      CURLE_COULDNT_RESOLVE_PROXY,
      CURLE_COULDNT_RESOLVE_HOST,
      CURLE_COULDNT_CONNECT,
      CURLE_FTP_WEIRD_SERVER_REPLY,
      CURLE_REMOTE_ACCESS_DENIED,
      CURLE_FTP_ACCEPT_FAILED,
      CURLE_FTP_WEIRD_PASS_REPLY,
      CURLE_FTP_ACCEPT_TIMEOUT,
      CURLE_FTP_WEIRD_PASV_REPLY,
      CURLE_FTP_WEIRD_227_FORMAT,
      CURLE_FTP_CANT_GET_HOST,
      CURLE_OBSOLETE16,
      CURLE_FTP_COULDNT_SET_TYPE,
      CURLE_PARTIAL_FILE,
      CURLE_FTP_COULDNT_RETR_FILE,
      CURLE_OBSOLETE20,
      CURLE_QUOTE_ERROR,
      CURLE_HTTP_RETURNED_ERROR,
      CURLE_WRITE_ERROR,
      CURLE_OBSOLETE24,
      CURLE_UPLOAD_FAILED,
      CURLE_READ_ERROR,
      CURLE_OUT_OF_MEMORY,
      CURLE_OPERATION_TIMEDOUT,
      CURLE_OBSOLETE29,
      CURLE_FTP_PORT_FAILED,
      CURLE_FTP_COULDNT_USE_REST,
      CURLE_OBSOLETE32,
      CURLE_RANGE_ERROR,
      CURLE_HTTP_POST_ERROR,
      CURLE_SSL_CONNECT_ERROR,
      CURLE_BAD_DOWNLOAD_RESUME,
      CURLE_FILE_COULDNT_READ_FILE,
      CURLE_LDAP_CANNOT_BIND,
      CURLE_LDAP_SEARCH_FAILED,
      CURLE_OBSOLETE40,
      CURLE_FUNCTION_NOT_FOUND,
      CURLE_ABORTED_BY_CALLBACK,
      CURLE_BAD_FUNCTION_ARGUMENT,
      CURLE_OBSOLETE44,
      CURLE_INTERFACE_FAILED,
      CURLE_OBSOLETE46,
      CURLE_TOO_MANY_REDIRECTS ,
      CURLE_UNKNOWN_OPTION,
      CURLE_TELNET_OPTION_SYNTAX ,
      CURLE_OBSOLETE50,
      CURLE_PEER_FAILED_VERIFICATION,
      CURLE_GOT_NOTHING,
      CURLE_SSL_ENGINE_NOTFOUND,
      CURLE_SSL_ENGINE_SETFAILED,
      CURLE_SEND_ERROR,
      CURLE_RECV_ERROR,
      CURLE_OBSOLETE57,
      CURLE_SSL_CERTPROBLEM,
      CURLE_SSL_CIPHER,
      CURLE_SSL_CACERT,
      CURLE_BAD_CONTENT_ENCODING,
      CURLE_LDAP_INVALID_URL,
      CURLE_FILESIZE_EXCEEDED,
      CURLE_USE_SSL_FAILED,
      CURLE_SEND_FAIL_REWIND,
      CURLE_SSL_ENGINE_INITFAILED,
      CURLE_LOGIN_DENIED,            
      CURLE_TFTP_NOTFOUND,
      CURLE_TFTP_PERM,
      CURLE_REMOTE_DISK_FULL,
      CURLE_TFTP_ILLEGAL,
      CURLE_TFTP_UNKNOWNID,
      CURLE_REMOTE_FILE_EXISTS,
      CURLE_TFTP_NOSUCHUSER,
      CURLE_CONV_FAILED,
      CURLE_CONV_REQD,
      CURLE_SSL_CACERT_BADFILE,
      CURLE_REMOTE_FILE_NOT_FOUND,
      CURLE_SSH,
      CURLE_SSL_SHUTDOWN_FAILED,
      CURLE_AGAIN,
      CURLE_SSL_CRL_BADFILE,
      CURLE_SSL_ISSUER_ERROR,
      CURLE_FTP_PRET_FAILED,
      CURLE_RTSP_CSEQ_ERROR,
      CURLE_RTSP_SESSION_ERROR,
      CURLE_FTP_BAD_FILE_LIST,
      CURLE_CHUNK_FAILED,
      CURLE_NO_CONNECTION_AVAILABLE,
      CURL_LAST
      );
(*  /* This prototype applies to all conversion callbacks */
typedef CURLcode (*curl_conv_callback)(char *buffer, size_t length);

typedef CURLcode (*curl_ssl_ctx_callback)(CURL *curl,    /* easy handle */
                                          void *ssl_ctx, /* actually an
                                                            OpenSSL SSL_CTX */
                                          void *userptr);
*)
  TCUrlConvCallback=function(buffer:PAnsiChar;length:Integer):TCUrlCode;cdecl;
  TCurlSSLCtxCallback=function(curl:THandle;ssl_ctx:Pointer;userptr:Pointer):TCUrlCode;cdecl;
  (*
  typedef enum {
  CURLPROXY_HTTP = 0,   /* added in 7.10, new in 7.19.4 default is to use
                           CONNECT HTTP/1.1 */
  CURLPROXY_HTTP_1_0 = 1,   /* added in 7.19.4, force to use CONNECT
                               HTTP/1.0  */
  CURLPROXY_SOCKS4 = 4, /* support added in 7.15.2, enum existed already
                           in 7.10 */
  CURLPROXY_SOCKS5 = 5, /* added in 7.10 */
  CURLPROXY_SOCKS4A = 6, /* added in 7.18.0 */
  CURLPROXY_SOCKS5_HOSTNAME = 7 /* Use the SOCKS5 protocol but pass along the
                                   host name rather than the IP address. added
                                   in 7.18.0 */
} curl_proxytype;  /* this enum was added in 7.10 */
*)
  TCUrlProxyType=(
    CURLPROXY_HTTP = 0,
    CURLPROXY_HTTP_1_0 = 1,
    CURLPROXY_SOCKS4 = 4,
    CURLPROXY_SOCKS5 = 5,
    CURLPROXY_SOCKS4A = 6,
    CURLPROXY_SOCKS5_HOSTNAME = 7
    );
(*
struct curl_khkey {
  const char *key; /* points to a zero-terminated string encoded with base64
                      if len is zero, otherwise to the "raw" data */
  size_t len;
  enum type {
    CURLKHTYPE_UNKNOWN,
    CURLKHTYPE_RSA1,
    CURLKHTYPE_RSA,
    CURLKHTYPE_DSS
  } keytype;
};
*)
  TCurlKeyType=(
    CURLKHTYPE_UNKNOWN,
    CURLKHTYPE_RSA1,
    CURLKHTYPE_RSA,
    CURLKHTYPE_DSS
    );
  TCurlKhKey=record
    key:PAnsiChar;
    len:Integer;
    keytype:TCurlKeyType;
  end;
(*
/* this is the set of return values expected from the curl_sshkeycallback
   callback */
enum curl_khstat {
  CURLKHSTAT_FINE_ADD_TO_FILE,
  CURLKHSTAT_FINE,
  CURLKHSTAT_REJECT, /* reject the connection, return an error */
  CURLKHSTAT_DEFER,  /* do not accept it, but we can't answer right now so
                        this causes a CURLE_DEFER error but otherwise the
                        connection will be left intact etc */
  CURLKHSTAT_LAST    /* not for use, only a marker for last-in-list */
};
*)
  TCurlKhStat=(
    CURLKHSTAT_FINE_ADD_TO_FILE,
    CURLKHSTAT_FINE,
    CURLKHSTAT_REJECT,
    CURLKHSTAT_DEFER,
    CURLKHSTAT_LAST
  );
(*
  /* this is the set of status codes pass in to the callback */
enum curl_khmatch {
  CURLKHMATCH_OK,       /* match */
  CURLKHMATCH_MISMATCH, /* host found, key mismatch! */
  CURLKHMATCH_MISSING,  /* no matching host/key found */
  CURLKHMATCH_LAST      /* not for use, only a marker for last-in-list */
};
*)
  TCurlKHMatch=(
    CURLKHMATCH_OK,
    CURLKHMATCH_MISMATCH,
    CURLKHMATCH_MISSING,
    CURLKHMATCH_LAST
  );
(*
  typedef int
  (*curl_sshkeycallback) (CURL *easy,     /* easy handle */
                          const struct curl_khkey *knownkey, /* known */
                          const struct curl_khkey *foundkey, /* found */
                          enum curl_khmatch, /* libcurl's view on the keys */
                          void *clientp); /* custom pointer passed from app */
*)
  TCurlSSHKeyCallback=function(handle:THandle;knownkey,foundkey:TCUrlKhKey;match:TCurlKHMatch;clientp:Pointer):Integer;cdecl;
(*
  /* parameter for the CURLOPT_USE_SSL option */
typedef enum {
  CURLUSESSL_NONE,    /* do not attempt to use SSL */
  CURLUSESSL_TRY,     /* try using SSL, proceed anyway otherwise */
  CURLUSESSL_CONTROL, /* SSL for the control connection or fail */
  CURLUSESSL_ALL,     /* SSL for all communication or fail */
  CURLUSESSL_LAST     /* not an option, never use */
} curl_usessl;
*)
  TCurlUseSSL=(
    CURLUSESSL_NONE,
    CURLUSESSL_TRY,
    CURLUSESSL_CONTROL,
    CURLUSESSL_ALL,
    CURLUSESSL_LAST
  );
(*
/* parameter for the CURLOPT_FTP_SSL_CCC option */
typedef enum {
  CURLFTPSSL_CCC_NONE,    /* do not send CCC */
  CURLFTPSSL_CCC_PASSIVE, /* Let the server initiate the shutdown */
  CURLFTPSSL_CCC_ACTIVE,  /* Initiate the shutdown */
  CURLFTPSSL_CCC_LAST     /* not an option, never use */
} curl_ftpccc;


*)
  TCurlFtpCCC=(
    CURLFTPSSL_CCC_NONE,
    CURLFTPSSL_CCC_PASSIVE,
    CURLFTPSSL_CCC_ACTIVE,
    CURLFTPSSL_CCC_LAST     
    );
(*
/* parameter for the CURLOPT_FTPSSLAUTH option */
typedef enum {
  CURLFTPAUTH_DEFAULT, /* let libcurl decide */
  CURLFTPAUTH_SSL,     /* use "AUTH SSL" */
  CURLFTPAUTH_TLS,     /* use "AUTH TLS" */
  CURLFTPAUTH_LAST /* not an option, never use */
} curl_ftpauth;

*)
  TCurlFTPAuth=(
    CURLFTPAUTH_DEFAULT,
    CURLFTPAUTH_SSL,
    CURLFTPAUTH_TLS,
    CURLFTPAUTH_LAST
  );
(*
/* parameter for the CURLOPT_FTP_CREATE_MISSING_DIRS option */
typedef enum {
  CURLFTP_CREATE_DIR_NONE,  /* do NOT create missing dirs! */
  CURLFTP_CREATE_DIR,       /* (FTP/SFTP) if CWD fails, try MKD and then CWD
                               again if MKD succeeded, for SFTP this does
                               similar magic */
  CURLFTP_CREATE_DIR_RETRY, /* (FTP only) if CWD fails, try MKD and then CWD
                               again even if MKD failed! */
  CURLFTP_CREATE_DIR_LAST   /* not an option, never use */
} curl_ftpcreatedir;
*)
  TCurlFTPCreateDir=(
    CURLFTP_CREATE_DIR_NONE,
    CURLFTP_CREATE_DIR,
    CURLFTP_CREATE_DIR_RETRY,
    CURLFTP_CREATE_DIR_LAST
  );
(*
/* parameter for the CURLOPT_FTP_FILEMETHOD option */
typedef enum {
  CURLFTPMETHOD_DEFAULT,   /* let libcurl pick */
  CURLFTPMETHOD_MULTICWD,  /* single CWD operation for each path part */
  CURLFTPMETHOD_NOCWD,     /* no CWD at all */
  CURLFTPMETHOD_SINGLECWD, /* one CWD to full dir, then work on file */
  CURLFTPMETHOD_LAST       /* not an option, never use */
} curl_ftpmethod;
*)
  TCurlFTPMethod=(
    CURLFTPMETHOD_DEFAULT,
    CURLFTPMETHOD_MULTICWD,
    CURLFTPMETHOD_NOCWD,
    CURLFTPMETHOD_SINGLECWD,
    CURLFTPMETHOD_LAST
  );

(*
/*
 * This macro-mania below setups the CURLOPT_[what] enum, to be used with
 * curl_easy_setopt(). The first argument in the CINIT() macro is the [what]
 * word.
 */

typedef enum {
  /* This is the FILE * or void * the regular output should be written to. */
  CINIT(FILE, OBJECTPOINT, 1),

  /* The full URL to get/put */
  CINIT(URL,  OBJECTPOINT, 2),

  /* Port number to connect to, if other than default. */
  CINIT(PORT, LONG, 3),

  /* Name of proxy to use. */
  CINIT(PROXY, OBJECTPOINT, 4),

  /* "name:password" to use when fetching. */
  CINIT(USERPWD, OBJECTPOINT, 5),

  /* "name:password" to use with proxy. */
  CINIT(PROXYUSERPWD, OBJECTPOINT, 6),

  /* Range to get, specified as an ASCII string. */
  CINIT(RANGE, OBJECTPOINT, 7),

  /* not used */

  /* Specified file stream to upload from (use as input): */
  CINIT(INFILE, OBJECTPOINT, 9),

  /* Buffer to receive error messages in, must be at least CURL_ERROR_SIZE
   * bytes big. If this is not used, error messages go to stderr instead: */
  CINIT(ERRORBUFFER, OBJECTPOINT, 10),

  /* Function that will be called to store the output (instead of fwrite). The
   * parameters will use fwrite() syntax, make sure to follow them. */
  CINIT(WRITEFUNCTION, FUNCTIONPOINT, 11),

  /* Function that will be called to read the input (instead of fread). The
   * parameters will use fread() syntax, make sure to follow them. */
  CINIT(READFUNCTION, FUNCTIONPOINT, 12),

  /* Time-out the read operation after this amount of seconds */
  CINIT(TIMEOUT, LONG, 13),

  /* If the CURLOPT_INFILE is used, this can be used to inform libcurl about
   * how large the file being sent really is. That allows better error
   * checking and better verifies that the upload was successful. -1 means
   * unknown size.
   *
   * For large file support, there is also a _LARGE version of the key
   * which takes an off_t type, allowing platforms with larger off_t
   * sizes to handle larger files.  See below for INFILESIZE_LARGE.
   */
  CINIT(INFILESIZE, LONG, 14),

  /* POST static input fields. */
  CINIT(POSTFIELDS, OBJECTPOINT, 15),

  /* Set the referrer page (needed by some CGIs) */
  CINIT(REFERER, OBJECTPOINT, 16),

  /* Set the FTP PORT string (interface name, named or numerical IP address)
     Use i.e '-' to use default address. */
  CINIT(FTPPORT, OBJECTPOINT, 17),

  /* Set the User-Agent string (examined by some CGIs) */
  CINIT(USERAGENT, OBJECTPOINT, 18),

  /* If the download receives less than "low speed limit" bytes/second
   * during "low speed time" seconds, the operations is aborted.
   * You could i.e if you have a pretty high speed connection, abort if
   * it is less than 2000 bytes/sec during 20 seconds.
   */

  /* Set the "low speed limit" */
  CINIT(LOW_SPEED_LIMIT, LONG, 19),

  /* Set the "low speed time" */
  CINIT(LOW_SPEED_TIME, LONG, 20),

  /* Set the continuation offset.
   *
   * Note there is also a _LARGE version of this key which uses
   * off_t types, allowing for large file offsets on platforms which
   * use larger-than-32-bit off_t's.  Look below for RESUME_FROM_LARGE.
   */
  CINIT(RESUME_FROM, LONG, 21),

  /* Set cookie in request: */
  CINIT(COOKIE, OBJECTPOINT, 22),

  /* This points to a linked list of headers, struct curl_slist kind */
  CINIT(HTTPHEADER, OBJECTPOINT, 23),

  /* This points to a linked list of post entries, struct curl_httppost */
  CINIT(HTTPPOST, OBJECTPOINT, 24),

  /* name of the file keeping your private SSL-certificate */
  CINIT(SSLCERT, OBJECTPOINT, 25),

  /* password for the SSL or SSH private key */
  CINIT(KEYPASSWD, OBJECTPOINT, 26),

  /* send TYPE parameter? */
  CINIT(CRLF, LONG, 27),

  /* send linked-list of QUOTE commands */
  CINIT(QUOTE, OBJECTPOINT, 28),

  /* send FILE * or void * to store headers to, if you use a callback it
     is simply passed to the callback unmodified */
  CINIT(WRITEHEADER, OBJECTPOINT, 29),

  /* point to a file to read the initial cookies from, also enables
     "cookie awareness" */
  CINIT(COOKIEFILE, OBJECTPOINT, 31),

  /* What version to specifically try to use.
     See CURL_SSLVERSION defines below. */
  CINIT(SSLVERSION, LONG, 32),

  /* What kind of HTTP time condition to use, see defines */
  CINIT(TIMECONDITION, LONG, 33),

  /* Time to use with the above condition. Specified in number of seconds
     since 1 Jan 1970 */
  CINIT(TIMEVALUE, LONG, 34),

  /* 35 = OBSOLETE */

  /* Custom request, for customizing the get command like
     HTTP: DELETE, TRACE and others
     FTP: to use a different list command
     */
  CINIT(CUSTOMREQUEST, OBJECTPOINT, 36),

  /* HTTP request, for odd commands like DELETE, TRACE and others */
  CINIT(STDERR, OBJECTPOINT, 37),

  /* 38 is not used */

  /* send linked-list of post-transfer QUOTE commands */
  CINIT(POSTQUOTE, OBJECTPOINT, 39),

  CINIT(WRITEINFO, OBJECTPOINT, 40), /* DEPRECATED, do not use! */

  CINIT(VERBOSE, LONG, 41),      /* talk a lot */
  CINIT(HEADER, LONG, 42),       /* throw the header out too */
  CINIT(NOPROGRESS, LONG, 43),   /* shut off the progress meter */
  CINIT(NOBODY, LONG, 44),       /* use HEAD to get http document */
  CINIT(FAILONERROR, LONG, 45),  /* no output on http error codes >= 300 */
  CINIT(UPLOAD, LONG, 46),       /* this is an upload */
  CINIT(POST, LONG, 47),         /* HTTP POST method */
  CINIT(DIRLISTONLY, LONG, 48),  /* bare names when listing directories */

  CINIT(APPEND, LONG, 50),       /* Append instead of overwrite on upload! */

  /* Specify whether to read the user+password from the .netrc or the URL.
   * This must be one of the CURL_NETRC_* enums below. */
  CINIT(NETRC, LONG, 51),

  CINIT(FOLLOWLOCATION, LONG, 52),  /* use Location: Luke! */

  CINIT(TRANSFERTEXT, LONG, 53), /* transfer data in text/ASCII format */
  CINIT(PUT, LONG, 54),          /* HTTP PUT */

  /* 55 = OBSOLETE */

  /* Function that will be called instead of the internal progress display
   * function. This function should be defined as the curl_progress_callback
   * prototype defines. */
  CINIT(PROGRESSFUNCTION, FUNCTIONPOINT, 56),

  /* Data passed to the progress callback */
  CINIT(PROGRESSDATA, OBJECTPOINT, 57),

  /* We want the referrer field set automatically when following locations */
  CINIT(AUTOREFERER, LONG, 58),

  /* Port of the proxy, can be set in the proxy string as well with:
     "[host]:[port]" */
  CINIT(PROXYPORT, LONG, 59),

  /* size of the POST input data, if strlen() is not good to use */
  CINIT(POSTFIELDSIZE, LONG, 60),

  /* tunnel non-http operations through a HTTP proxy */
  CINIT(HTTPPROXYTUNNEL, LONG, 61),

  /* Set the interface string to use as outgoing network interface */
  CINIT(INTERFACE, OBJECTPOINT, 62),

  /* Set the krb4/5 security level, this also enables krb4/5 awareness.  This
   * is a string, 'clear', 'safe', 'confidential' or 'private'.  If the string
   * is set but doesn't match one of these, 'private' will be used.  */
  CINIT(KRBLEVEL, OBJECTPOINT, 63),

  /* Set if we should verify the peer in ssl handshake, set 1 to verify. */
  CINIT(SSL_VERIFYPEER, LONG, 64),

  /* The CApath or CAfile used to validate the peer certificate
     this option is used only if SSL_VERIFYPEER is true */
  CINIT(CAINFO, OBJECTPOINT, 65),

  /* 66 = OBSOLETE */
  /* 67 = OBSOLETE */

  /* Maximum number of http redirects to follow */
  CINIT(MAXREDIRS, LONG, 68),

  /* Pass a long set to 1 to get the date of the requested document (if
     possible)! Pass a zero to shut it off. */
  CINIT(FILETIME, LONG, 69),

  /* This points to a linked list of telnet options */
  CINIT(TELNETOPTIONS, OBJECTPOINT, 70),

  /* Max amount of cached alive connections */
  CINIT(MAXCONNECTS, LONG, 71),

  CINIT(CLOSEPOLICY, LONG, 72), /* DEPRECATED, do not use! */

  /* 73 = OBSOLETE */

  /* Set to explicitly use a new connection for the upcoming transfer.
     Do not use this unless you're absolutely sure of this, as it makes the
     operation slower and is less friendly for the network. */
  CINIT(FRESH_CONNECT, LONG, 74),

  /* Set to explicitly forbid the upcoming transfer's connection to be re-used
     when done. Do not use this unless you're absolutely sure of this, as it
     makes the operation slower and is less friendly for the network. */
  CINIT(FORBID_REUSE, LONG, 75),

  /* Set to a file name that contains random data for libcurl to use to
     seed the random engine when doing SSL connects. */
  CINIT(RANDOM_FILE, OBJECTPOINT, 76),

  /* Set to the Entropy Gathering Daemon socket pathname */
  CINIT(EGDSOCKET, OBJECTPOINT, 77),

  /* Time-out connect operations after this amount of seconds, if connects are
     OK within this time, then fine... This only aborts the connect phase. */
  CINIT(CONNECTTIMEOUT, LONG, 78),

  /* Function that will be called to store headers (instead of fwrite). The
   * parameters will use fwrite() syntax, make sure to follow them. */
  CINIT(HEADERFUNCTION, FUNCTIONPOINT, 79),

  /* Set this to force the HTTP request to get back to GET. Only really usable
     if POST, PUT or a custom request have been used first.
   */
  CINIT(HTTPGET, LONG, 80),

  /* Set if we should verify the Common name from the peer certificate in ssl
   * handshake, set 1 to check existence, 2 to ensure that it matches the
   * provided hostname. */
  CINIT(SSL_VERIFYHOST, LONG, 81),

  /* Specify which file name to write all known cookies in after completed
     operation. Set file name to "-" (dash) to make it go to stdout. */
  CINIT(COOKIEJAR, OBJECTPOINT, 82),

  /* Specify which SSL ciphers to use */
  CINIT(SSL_CIPHER_LIST, OBJECTPOINT, 83),

  /* Specify which HTTP version to use! This must be set to one of the
     CURL_HTTP_VERSION* enums set below. */
  CINIT(HTTP_VERSION, LONG, 84),

  /* Specifically switch on or off the FTP engine's use of the EPSV command. By
     default, that one will always be attempted before the more traditional
     PASV command. */
  CINIT(FTP_USE_EPSV, LONG, 85),

  /* type of the file keeping your SSL-certificate ("DER", "PEM", "ENG") */
  CINIT(SSLCERTTYPE, OBJECTPOINT, 86),

  /* name of the file keeping your private SSL-key */
  CINIT(SSLKEY, OBJECTPOINT, 87),

  /* type of the file keeping your private SSL-key ("DER", "PEM", "ENG") */
  CINIT(SSLKEYTYPE, OBJECTPOINT, 88),

  /* crypto engine for the SSL-sub system */
  CINIT(SSLENGINE, OBJECTPOINT, 89),

  /* set the crypto engine for the SSL-sub system as default
     the param has no meaning...
   */
  CINIT(SSLENGINE_DEFAULT, LONG, 90),

  /* Non-zero value means to use the global dns cache */
  CINIT(DNS_USE_GLOBAL_CACHE, LONG, 91), /* DEPRECATED, do not use! */

  /* DNS cache timeout */
  CINIT(DNS_CACHE_TIMEOUT, LONG, 92),

  /* send linked-list of pre-transfer QUOTE commands */
  CINIT(PREQUOTE, OBJECTPOINT, 93),

  /* set the debug function */
  CINIT(DEBUGFUNCTION, FUNCTIONPOINT, 94),

  /* set the data for the debug function */
  CINIT(DEBUGDATA, OBJECTPOINT, 95),

  /* mark this as start of a cookie session */
  CINIT(COOKIESESSION, LONG, 96),

  /* The CApath directory used to validate the peer certificate
     this option is used only if SSL_VERIFYPEER is true */
  CINIT(CAPATH, OBJECTPOINT, 97),

  /* Instruct libcurl to use a smaller receive buffer */
  CINIT(BUFFERSIZE, LONG, 98),

  /* Instruct libcurl to not use any signal/alarm handlers, even when using
     timeouts. This option is useful for multi-threaded applications.
     See libcurl-the-guide for more background information. */
  CINIT(NOSIGNAL, LONG, 99),

  /* Provide a CURLShare for mutexing non-ts data */
  CINIT(SHARE, OBJECTPOINT, 100),

  /* indicates type of proxy. accepted values are CURLPROXY_HTTP (default),
     CURLPROXY_SOCKS4, CURLPROXY_SOCKS4A and CURLPROXY_SOCKS5. */
  CINIT(PROXYTYPE, LONG, 101),

  /* Set the Accept-Encoding string. Use this to tell a server you would like
     the response to be compressed. Before 7.21.6, this was known as
     CURLOPT_ENCODING */
  CINIT(ACCEPT_ENCODING, OBJECTPOINT, 102),

  /* Set pointer to private data */
  CINIT(PRIVATE, OBJECTPOINT, 103),

  /* Set aliases for HTTP 200 in the HTTP Response header */
  CINIT(HTTP200ALIASES, OBJECTPOINT, 104),

  /* Continue to send authentication (user+password) when following locations,
     even when hostname changed. This can potentially send off the name
     and password to whatever host the server decides. */
  CINIT(UNRESTRICTED_AUTH, LONG, 105),

  /* Specifically switch on or off the FTP engine's use of the EPRT command (
     it also disables the LPRT attempt). By default, those ones will always be
     attempted before the good old traditional PORT command. */
  CINIT(FTP_USE_EPRT, LONG, 106),

  /* Set this to a bitmask value to enable the particular authentications
     methods you like. Use this in combination with CURLOPT_USERPWD.
     Note that setting multiple bits may cause extra network round-trips. */
  CINIT(HTTPAUTH, LONG, 107),

  /* Set the ssl context callback function, currently only for OpenSSL ssl_ctx
     in second argument. The function must be matching the
     curl_ssl_ctx_callback proto. */
  CINIT(SSL_CTX_FUNCTION, FUNCTIONPOINT, 108),

  /* Set the userdata for the ssl context callback function's third
     argument */
  CINIT(SSL_CTX_DATA, OBJECTPOINT, 109),

  /* FTP Option that causes missing dirs to be created on the remote server.
     In 7.19.4 we introduced the convenience enums for this option using the
     CURLFTP_CREATE_DIR prefix.
  */
  CINIT(FTP_CREATE_MISSING_DIRS, LONG, 110),

  /* Set this to a bitmask value to enable the particular authentications
     methods you like. Use this in combination with CURLOPT_PROXYUSERPWD.
     Note that setting multiple bits may cause extra network round-trips. */
  CINIT(PROXYAUTH, LONG, 111),

  /* FTP option that changes the timeout, in seconds, associated with
     getting a response.  This is different from transfer timeout time and
     essentially places a demand on the FTP server to acknowledge commands
     in a timely manner. */
  CINIT(FTP_RESPONSE_TIMEOUT, LONG, 112),
#define CURLOPT_SERVER_RESPONSE_TIMEOUT CURLOPT_FTP_RESPONSE_TIMEOUT

  /* Set this option to one of the CURL_IPRESOLVE_* defines (see below) to
     tell libcurl to resolve names to those IP versions only. This only has
     affect on systems with support for more than one, i.e IPv4 _and_ IPv6. */
  CINIT(IPRESOLVE, LONG, 113),

  /* Set this option to limit the size of a file that will be downloaded from
     an HTTP or FTP server.

     Note there is also _LARGE version which adds large file support for
     platforms which have larger off_t sizes.  See MAXFILESIZE_LARGE below. */
  CINIT(MAXFILESIZE, LONG, 114),

  /* See the comment for INFILESIZE above, but in short, specifies
   * the size of the file being uploaded.  -1 means unknown.
   */
  CINIT(INFILESIZE_LARGE, OFF_T, 115),

  /* Sets the continuation offset.  There is also a LONG version of this;
   * look above for RESUME_FROM.
   */
  CINIT(RESUME_FROM_LARGE, OFF_T, 116),

  /* Sets the maximum size of data that will be downloaded from
   * an HTTP or FTP server.  See MAXFILESIZE above for the LONG version.
   */
  CINIT(MAXFILESIZE_LARGE, OFF_T, 117),

  /* Set this option to the file name of your .netrc file you want libcurl
     to parse (using the CURLOPT_NETRC option). If not set, libcurl will do
     a poor attempt to find the user's home directory and check for a .netrc
     file in there. */
  CINIT(NETRC_FILE, OBJECTPOINT, 118),

  /* Enable SSL/TLS for FTP, pick one of:
     CURLUSESSL_TRY     - try using SSL, proceed anyway otherwise
     CURLUSESSL_CONTROL - SSL for the control connection or fail
     CURLUSESSL_ALL     - SSL for all communication or fail
  */
  CINIT(USE_SSL, LONG, 119),

  /* The _LARGE version of the standard POSTFIELDSIZE option */
  CINIT(POSTFIELDSIZE_LARGE, OFF_T, 120),

  /* Enable/disable the TCP Nagle algorithm */
  CINIT(TCP_NODELAY, LONG, 121),

  /* 122 OBSOLETE, used in 7.12.3. Gone in 7.13.0 */
  /* 123 OBSOLETE. Gone in 7.16.0 */
  /* 124 OBSOLETE, used in 7.12.3. Gone in 7.13.0 */
  /* 125 OBSOLETE, used in 7.12.3. Gone in 7.13.0 */
  /* 126 OBSOLETE, used in 7.12.3. Gone in 7.13.0 */
  /* 127 OBSOLETE. Gone in 7.16.0 */
  /* 128 OBSOLETE. Gone in 7.16.0 */

  /* When FTP over SSL/TLS is selected (with CURLOPT_USE_SSL), this option
     can be used to change libcurl's default action which is to first try
     "AUTH SSL" and then "AUTH TLS" in this order, and proceed when a OK
     response has been received.

     Available parameters are:
     CURLFTPAUTH_DEFAULT - let libcurl decide
     CURLFTPAUTH_SSL     - try "AUTH SSL" first, then TLS
     CURLFTPAUTH_TLS     - try "AUTH TLS" first, then SSL
  */
  CINIT(FTPSSLAUTH, LONG, 129),

  CINIT(IOCTLFUNCTION, FUNCTIONPOINT, 130),
  CINIT(IOCTLDATA, OBJECTPOINT, 131),

  /* 132 OBSOLETE. Gone in 7.16.0 */
  /* 133 OBSOLETE. Gone in 7.16.0 */

  /* zero terminated string for pass on to the FTP server when asked for
     "account" info */
  CINIT(FTP_ACCOUNT, OBJECTPOINT, 134),

  /* feed cookies into cookie engine */
  CINIT(COOKIELIST, OBJECTPOINT, 135),

  /* ignore Content-Length */
  CINIT(IGNORE_CONTENT_LENGTH, LONG, 136),

  /* Set to non-zero to skip the IP address received in a 227 PASV FTP server
     response. Typically used for FTP-SSL purposes but is not restricted to
     that. libcurl will then instead use the same IP address it used for the
     control connection. */
  CINIT(FTP_SKIP_PASV_IP, LONG, 137),

  /* Select "file method" to use when doing FTP, see the curl_ftpmethod
     above. */
  CINIT(FTP_FILEMETHOD, LONG, 138),

  /* Local port number to bind the socket to */
  CINIT(LOCALPORT, LONG, 139),

  /* Number of ports to try, including the first one set with LOCALPORT.
     Thus, setting it to 1 will make no additional attempts but the first.
  */
  CINIT(LOCALPORTRANGE, LONG, 140),

  /* no transfer, set up connection and let application use the socket by
     extracting it with CURLINFO_LASTSOCKET */
  CINIT(CONNECT_ONLY, LONG, 141),

  /* Function that will be called to convert from the
     network encoding (instead of using the iconv calls in libcurl) */
  CINIT(CONV_FROM_NETWORK_FUNCTION, FUNCTIONPOINT, 142),

  /* Function that will be called to convert to the
     network encoding (instead of using the iconv calls in libcurl) */
  CINIT(CONV_TO_NETWORK_FUNCTION, FUNCTIONPOINT, 143),

  /* Function that will be called to convert from UTF8
     (instead of using the iconv calls in libcurl)
     Note that this is used only for SSL certificate processing */
  CINIT(CONV_FROM_UTF8_FUNCTION, FUNCTIONPOINT, 144),

  /* if the connection proceeds too quickly then need to slow it down */
  /* limit-rate: maximum number of bytes per second to send or receive */
  CINIT(MAX_SEND_SPEED_LARGE, OFF_T, 145),
  CINIT(MAX_RECV_SPEED_LARGE, OFF_T, 146),

  /* Pointer to command string to send if USER/PASS fails. */
  CINIT(FTP_ALTERNATIVE_TO_USER, OBJECTPOINT, 147),

  /* callback function for setting socket options */
  CINIT(SOCKOPTFUNCTION, FUNCTIONPOINT, 148),
  CINIT(SOCKOPTDATA, OBJECTPOINT, 149),

  /* set to 0 to disable session ID re-use for this transfer, default is
     enabled (== 1) */
  CINIT(SSL_SESSIONID_CACHE, LONG, 150),

  /* allowed SSH authentication methods */
  CINIT(SSH_AUTH_TYPES, LONG, 151),

  /* Used by scp/sftp to do public/private key authentication */
  CINIT(SSH_PUBLIC_KEYFILE, OBJECTPOINT, 152),
  CINIT(SSH_PRIVATE_KEYFILE, OBJECTPOINT, 153),

  /* Send CCC (Clear Command Channel) after authentication */
  CINIT(FTP_SSL_CCC, LONG, 154),

  /* Same as TIMEOUT and CONNECTTIMEOUT, but with ms resolution */
  CINIT(TIMEOUT_MS, LONG, 155),
  CINIT(CONNECTTIMEOUT_MS, LONG, 156),

  /* set to zero to disable the libcurl's decoding and thus pass the raw body
     data to the application even when it is encoded/compressed */
  CINIT(HTTP_TRANSFER_DECODING, LONG, 157),
  CINIT(HTTP_CONTENT_DECODING, LONG, 158),

  /* Permission used when creating new files and directories on the remote
     server for protocols that support it, SFTP/SCP/FILE */
  CINIT(NEW_FILE_PERMS, LONG, 159),
  CINIT(NEW_DIRECTORY_PERMS, LONG, 160),

  /* Set the behaviour of POST when redirecting. Values must be set to one
     of CURL_REDIR* defines below. This used to be called CURLOPT_POST301 */
  CINIT(POSTREDIR, LONG, 161),

  /* used by scp/sftp to verify the host's public key */
  CINIT(SSH_HOST_PUBLIC_KEY_MD5, OBJECTPOINT, 162),

  /* Callback function for opening socket (instead of socket(2)). Optionally,
     callback is able change the address or refuse to connect returning
     CURL_SOCKET_BAD.  The callback should have type
     curl_opensocket_callback */
  CINIT(OPENSOCKETFUNCTION, FUNCTIONPOINT, 163),
  CINIT(OPENSOCKETDATA, OBJECTPOINT, 164),

  /* POST volatile input fields. */
  CINIT(COPYPOSTFIELDS, OBJECTPOINT, 165),

  /* set transfer mode (;type=<a|i>) when doing FTP via an HTTP proxy */
  CINIT(PROXY_TRANSFER_MODE, LONG, 166),

  /* Callback function for seeking in the input stream */
  CINIT(SEEKFUNCTION, FUNCTIONPOINT, 167),
  CINIT(SEEKDATA, OBJECTPOINT, 168),

  /* CRL file */
  CINIT(CRLFILE, OBJECTPOINT, 169),

  /* Issuer certificate */
  CINIT(ISSUERCERT, OBJECTPOINT, 170),

  /* (IPv6) Address scope */
  CINIT(ADDRESS_SCOPE, LONG, 171),

  /* Collect certificate chain info and allow it to get retrievable with
     CURLINFO_CERTINFO after the transfer is complete. (Unfortunately) only
     working with OpenSSL-powered builds. */
  CINIT(CERTINFO, LONG, 172),

  /* "name" and "pwd" to use when fetching. */
  CINIT(USERNAME, OBJECTPOINT, 173),
  CINIT(PASSWORD, OBJECTPOINT, 174),

    /* "name" and "pwd" to use with Proxy when fetching. */
  CINIT(PROXYUSERNAME, OBJECTPOINT, 175),
  CINIT(PROXYPASSWORD, OBJECTPOINT, 176),

  /* Comma separated list of hostnames defining no-proxy zones. These should
     match both hostnames directly, and hostnames within a domain. For
     example, local.com will match local.com and www.local.com, but NOT
     notlocal.com or www.notlocal.com. For compatibility with other
     implementations of this, .local.com will be considered to be the same as
     local.com. A single * is the only valid wildcard, and effectively
     disables the use of proxy. */
  CINIT(NOPROXY, OBJECTPOINT, 177),

  /* block size for TFTP transfers */
  CINIT(TFTP_BLKSIZE, LONG, 178),

  /* Socks Service */
  CINIT(SOCKS5_GSSAPI_SERVICE, OBJECTPOINT, 179),

  /* Socks Service */
  CINIT(SOCKS5_GSSAPI_NEC, LONG, 180),

  /* set the bitmask for the protocols that are allowed to be used for the
     transfer, which thus helps the app which takes URLs from users or other
     external inputs and want to restrict what protocol(s) to deal
     with. Defaults to CURLPROTO_ALL. */
  CINIT(PROTOCOLS, LONG, 181),

  /* set the bitmask for the protocols that libcurl is allowed to follow to,
     as a subset of the CURLOPT_PROTOCOLS ones. That means the protocol needs
     to be set in both bitmasks to be allowed to get redirected to. Defaults
     to all protocols except FILE and SCP. */
  CINIT(REDIR_PROTOCOLS, LONG, 182),

  /* set the SSH knownhost file name to use */
  CINIT(SSH_KNOWNHOSTS, OBJECTPOINT, 183),

  /* set the SSH host key callback, must point to a curl_sshkeycallback
     function */
  CINIT(SSH_KEYFUNCTION, FUNCTIONPOINT, 184),

  /* set the SSH host key callback custom pointer */
  CINIT(SSH_KEYDATA, OBJECTPOINT, 185),

  /* set the SMTP mail originator */
  CINIT(MAIL_FROM, OBJECTPOINT, 186),

  /* set the SMTP mail receiver(s) */
  CINIT(MAIL_RCPT, OBJECTPOINT, 187),

  /* FTP: send PRET before PASV */
  CINIT(FTP_USE_PRET, LONG, 188),

  /* RTSP request method (OPTIONS, SETUP, PLAY, etc...) */
  CINIT(RTSP_REQUEST, LONG, 189),

  /* The RTSP session identifier */
  CINIT(RTSP_SESSION_ID, OBJECTPOINT, 190),

  /* The RTSP stream URI */
  CINIT(RTSP_STREAM_URI, OBJECTPOINT, 191),

  /* The Transport: header to use in RTSP requests */
  CINIT(RTSP_TRANSPORT, OBJECTPOINT, 192),

  /* Manually initialize the client RTSP CSeq for this handle */
  CINIT(RTSP_CLIENT_CSEQ, LONG, 193),

  /* Manually initialize the server RTSP CSeq for this handle */
  CINIT(RTSP_SERVER_CSEQ, LONG, 194),

  /* The stream to pass to INTERLEAVEFUNCTION. */
  CINIT(INTERLEAVEDATA, OBJECTPOINT, 195),

  /* Let the application define a custom write method for RTP data */
  CINIT(INTERLEAVEFUNCTION, FUNCTIONPOINT, 196),

  /* Turn on wildcard matching */
  CINIT(WILDCARDMATCH, LONG, 197),

  /* Directory matching callback called before downloading of an
     individual file (chunk) started */
  CINIT(CHUNK_BGN_FUNCTION, FUNCTIONPOINT, 198),

  /* Directory matching callback called after the file (chunk)
     was downloaded, or skipped */
  CINIT(CHUNK_END_FUNCTION, FUNCTIONPOINT, 199),

  /* Change match (fnmatch-like) callback for wildcard matching */
  CINIT(FNMATCH_FUNCTION, FUNCTIONPOINT, 200),

  /* Let the application define custom chunk data pointer */
  CINIT(CHUNK_DATA, OBJECTPOINT, 201),

  /* FNMATCH_FUNCTION user pointer */
  CINIT(FNMATCH_DATA, OBJECTPOINT, 202),

  /* send linked-list of name:port:address sets */
  CINIT(RESOLVE, OBJECTPOINT, 203),

  /* Set a username for authenticated TLS */
  CINIT(TLSAUTH_USERNAME, OBJECTPOINT, 204),

  /* Set a password for authenticated TLS */
  CINIT(TLSAUTH_PASSWORD, OBJECTPOINT, 205),

  /* Set authentication type for authenticated TLS */
  CINIT(TLSAUTH_TYPE, OBJECTPOINT, 206),

  /* Set to 1 to enable the "TE:" header in HTTP requests to ask for
     compressed transfer-encoded responses. Set to 0 to disable the use of TE:
     in outgoing requests. The current default is 0, but it might change in a
     future libcurl release.

     libcurl will ask for the compressed methods it knows of, and if that
     isn't any, it will not ask for transfer-encoding at all even if this
     option is set to 1.

  */
  CINIT(TRANSFER_ENCODING, LONG, 207),

  /* Callback function for closing socket (instead of close(2)). The callback
     should have type curl_closesocket_callback */
  CINIT(CLOSESOCKETFUNCTION, FUNCTIONPOINT, 208),
  CINIT(CLOSESOCKETDATA, OBJECTPOINT, 209),

  /* allow GSSAPI credential delegation */
  CINIT(GSSAPI_DELEGATION, LONG, 210),

  /* Set the name servers to use for DNS resolution */
  CINIT(DNS_SERVERS, OBJECTPOINT, 211),

  /* Time-out accept operations (currently for FTP only) after this amount
     of miliseconds. */
  CINIT(ACCEPTTIMEOUT_MS, LONG, 212),

  /* Set TCP keepalive */
  CINIT(TCP_KEEPALIVE, LONG, 213),

  /* non-universal keepalive knobs (Linux, AIX, HP-UX, more) */
  CINIT(TCP_KEEPIDLE, LONG, 214),
  CINIT(TCP_KEEPINTVL, LONG, 215),

  /* Enable/disable specific SSL features with a bitmask, see CURLSSLOPT_* */
  CINIT(SSL_OPTIONS, LONG, 216),

  /* set the SMTP auth originator */
  CINIT(MAIL_AUTH, OBJECTPOINT, 217),

  CURLOPT_LASTENTRY /* the last unused */
} CURLoption;
*)
  TCUrlOption=(
    CURLOPT_FILE=CURLOPTTYPE_OBJECTPOINT+1,
    CURLOPT_URL=CURLOPTTYPE_OBJECTPOINT+2,
    CURLOPT_PORT=CURLOPTTYPE_LONG+3,
    CURLOPT_PROXY=CURLOPTTYPE_OBJECTPOINT+4,
    CURLOPT_USERPWD=CURLOPTTYPE_OBJECTPOINT+5,
    CURLOPT_PROXYUSERPWD=CURLOPTTYPE_OBJECTPOINT+6,
    CURLOPT_RANGE=CURLOPTTYPE_OBJECTPOINT+7,
    CURLOPT_INFILE=CURLOPTTYPE_OBJECTPOINT+9,
    CURLOPT_ERRORBUFFER=CURLOPTTYPE_OBJECTPOINT+10,
    CURLOPT_WRITEFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+11,
    CURLOPT_READFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+12,
    CURLOPT_TIMEOUT=CURLOPTTYPE_LONG+13,
    CURLOPT_INFILESIZE=CURLOPTTYPE_LONG+14,
    CURLOPT_POSTFIELDS=CURLOPTTYPE_OBJECTPOINT+15,
    CURLOPT_REFERER=CURLOPTTYPE_OBJECTPOINT+16,
    CURLOPT_FTPPORT=CURLOPTTYPE_OBJECTPOINT+17,
    CURLOPT_USERAGENT=CURLOPTTYPE_OBJECTPOINT+18,
    CURLOPT_LOW_SPEED_LIMIT=CURLOPTTYPE_LONG+19,
    CURLOPT_LOW_SPEED_TIME=CURLOPTTYPE_LONG+20,
    CURLOPT_RESUME_FROM=CURLOPTTYPE_LONG+21,
    CURLOPT_COOKIE=CURLOPTTYPE_OBJECTPOINT+22,
    CURLOPT_HTTPHEADER=CURLOPTTYPE_OBJECTPOINT+23,
    CURLOPT_HTTPPOST=CURLOPTTYPE_OBJECTPOINT+24,
    CURLOPT_SSLCERT=CURLOPTTYPE_OBJECTPOINT+25,
    CURLOPT_KEYPASSWD=CURLOPTTYPE_OBJECTPOINT+26,
    CURLOPT_CRLF=CURLOPTTYPE_LONG+27,
    CURLOPT_QUOTE=CURLOPTTYPE_OBJECTPOINT+28,
    CURLOPT_WRITEHEADER=CURLOPTTYPE_OBJECTPOINT+29,
    CURLOPT_COOKIEFILE=CURLOPTTYPE_OBJECTPOINT+31,
    CURLOPT_SSLVERSION=CURLOPTTYPE_LONG+32,
    CURLOPT_TIMECONDITION=CURLOPTTYPE_LONG+33,
    CURLOPT_TIMEVALUE=CURLOPTTYPE_LONG+34,
    CURLOPT_CUSTOMREQUEST=CURLOPTTYPE_OBJECTPOINT+36,
    CURLOPT_STDERR=CURLOPTTYPE_OBJECTPOINT+37,
    CURLOPT_POSTQUOTE=CURLOPTTYPE_OBJECTPOINT+39,
    CURLOPT_WRITEINFO=CURLOPTTYPE_OBJECTPOINT+40,
    CURLOPT_VERBOSE=CURLOPTTYPE_LONG+41,
    CURLOPT_HEADER=CURLOPTTYPE_LONG+42,
    CURLOPT_NOPROGRESS=CURLOPTTYPE_LONG+43,
    CURLOPT_NOBODY=CURLOPTTYPE_LONG+44,
    CURLOPT_FAILONERROR=CURLOPTTYPE_LONG+45,
    CURLOPT_UPLOAD=CURLOPTTYPE_LONG+46,
    CURLOPT_POST=CURLOPTTYPE_LONG+47,
    CURLOPT_DIRLISTONLY=CURLOPTTYPE_LONG+48,
    CURLOPT_APPEND=CURLOPTTYPE_LONG+50,
    CURLOPT_NETRC=CURLOPTTYPE_LONG+51,
    CURLOPT_FOLLOWLOCATION=CURLOPTTYPE_LONG+52,
    CURLOPT_TRANSFERTEXT=CURLOPTTYPE_LONG+53,
    CURLOPT_PUT=CURLOPTTYPE_LONG+54,
    CURLOPT_PROGRESSFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+56,
    CURLOPT_PROGRESSDATA=CURLOPTTYPE_OBJECTPOINT+57,
    CURLOPT_AUTOREFERER=CURLOPTTYPE_LONG+58,
    CURLOPT_PROXYPORT=CURLOPTTYPE_LONG+59,
    CURLOPT_POSTFIELDSIZE=CURLOPTTYPE_LONG+60,
    CURLOPT_HTTPPROXYTUNNEL=CURLOPTTYPE_LONG+61,
    CURLOPT_INTERFACE=CURLOPTTYPE_OBJECTPOINT+62,
    CURLOPT_KRBLEVEL=CURLOPTTYPE_OBJECTPOINT+63,
    CURLOPT_SSL_VERIFYPEER=CURLOPTTYPE_LONG+64,
    CURLOPT_CAINFO=CURLOPTTYPE_OBJECTPOINT+65,
    CURLOPT_MAXREDIRS=CURLOPTTYPE_LONG+68,
    CURLOPT_FILETIME=CURLOPTTYPE_LONG+69,
    CURLOPT_TELNETOPTIONS=CURLOPTTYPE_OBJECTPOINT+70,
    CURLOPT_MAXCONNECTS=CURLOPTTYPE_LONG+71,
    CURLOPT_CLOSEPOLICY=CURLOPTTYPE_LONG+72,
    CURLOPT_FRESH_CONNECT=CURLOPTTYPE_LONG+74,
    CURLOPT_FORBID_REUSE=CURLOPTTYPE_LONG+75,
    CURLOPT_RANDOM_FILE=CURLOPTTYPE_OBJECTPOINT+76,
    CURLOPT_EGDSOCKET=CURLOPTTYPE_OBJECTPOINT+77,
    CURLOPT_CONNECTTIMEOUT=CURLOPTTYPE_LONG+78,
    CURLOPT_HEADERFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+79,
    CURLOPT_HTTPGET=CURLOPTTYPE_LONG+80,
    CURLOPT_SSL_VERIFYHOST=CURLOPTTYPE_LONG+81,
    CURLOPT_COOKIEJAR=CURLOPTTYPE_OBJECTPOINT+82,
    CURLOPT_SSL_CIPHER_LIST=CURLOPTTYPE_OBJECTPOINT+83,
    CURLOPT_HTTP_VERSION=CURLOPTTYPE_LONG+84,
    CURLOPT_FTP_USE_EPSV=CURLOPTTYPE_LONG+85,
    CURLOPT_SSLCERTTYPE=CURLOPTTYPE_OBJECTPOINT+86,
    CURLOPT_SSLKEY=CURLOPTTYPE_OBJECTPOINT+87,
    CURLOPT_SSLKEYTYPE=CURLOPTTYPE_OBJECTPOINT+88,
    CURLOPT_SSLENGINE=CURLOPTTYPE_OBJECTPOINT+89,
    CURLOPT_SSLENGINE_DEFAULT=CURLOPTTYPE_LONG+90,
    CURLOPT_DNS_USE_GLOBAL_CACHE=CURLOPTTYPE_LONG+91,
    CURLOPT_DNS_CACHE_TIMEOUT=CURLOPTTYPE_LONG+92,
    CURLOPT_PREQUOTE=CURLOPTTYPE_OBJECTPOINT+93,
    CURLOPT_DEBUGFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+94,
    CURLOPT_DEBUGDATA=CURLOPTTYPE_OBJECTPOINT+95,
    CURLOPT_COOKIESESSION=CURLOPTTYPE_LONG+96,
    CURLOPT_CAPATH=CURLOPTTYPE_OBJECTPOINT+97,
    CURLOPT_BUFFERSIZE=CURLOPTTYPE_LONG+98,
    CURLOPT_NOSIGNAL=CURLOPTTYPE_LONG+99,
    CURLOPT_SHARE=CURLOPTTYPE_OBJECTPOINT+100,
    CURLOPT_PROXYTYPE=CURLOPTTYPE_LONG+101,
    CURLOPT_ACCEPT_ENCODING=CURLOPTTYPE_OBJECTPOINT+102,
    CURLOPT_PRIVATE=CURLOPTTYPE_OBJECTPOINT+103,
    CURLOPT_HTTP200ALIASES=CURLOPTTYPE_OBJECTPOINT+104,
    CURLOPT_UNRESTRICTED_AUTH=CURLOPTTYPE_LONG+105,
    CURLOPT_FTP_USE_EPRT=CURLOPTTYPE_LONG+106,
    CURLOPT_HTTPAUTH=CURLOPTTYPE_LONG+107,
    CURLOPT_SSL_CTX_FUNCTION=CURLOPTTYPE_FUNCTIONPOINT+108,
    CURLOPT_SSL_CTX_DATA=CURLOPTTYPE_OBJECTPOINT+109,
    CURLOPT_FTP_CREATE_MISSING_DIRS=CURLOPTTYPE_LONG+110,
    CURLOPT_PROXYAUTH=CURLOPTTYPE_LONG+111,
    CURLOPT_FTP_RESPONSE_TIMEOUT=CURLOPTTYPE_LONG+112,
    CURLOPT_IPRESOLVE=CURLOPTTYPE_LONG+113,
    CURLOPT_MAXFILESIZE=CURLOPTTYPE_LONG+114,
    CURLOPT_INFILESIZE_LARGE=CURLOPTTYPE_OFF_T+115,
    CURLOPT_RESUME_FROM_LARGE=CURLOPTTYPE_OFF_T+116,
    CURLOPT_MAXFILESIZE_LARGE=CURLOPTTYPE_OFF_T+117,
    CURLOPT_NETRC_FILE=CURLOPTTYPE_OBJECTPOINT+118,
    CURLOPT_USE_SSL=CURLOPTTYPE_LONG+119,
    CURLOPT_POSTFIELDSIZE_LARGE=CURLOPTTYPE_OFF_T+120,
    CURLOPT_TCP_NODELAY=CURLOPTTYPE_LONG+121,
    CURLOPT_FTPSSLAUTH=CURLOPTTYPE_LONG+129,
    CURLOPT_IOCTLFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+130,
    CURLOPT_IOCTLDATA=CURLOPTTYPE_OBJECTPOINT+131,
    CURLOPT_FTP_ACCOUNT=CURLOPTTYPE_OBJECTPOINT+134,
    CURLOPT_COOKIELIST=CURLOPTTYPE_OBJECTPOINT+135,
    CURLOPT_IGNORE_CONTENT_LENGTH=CURLOPTTYPE_LONG+136,
    CURLOPT_FTP_SKIP_PASV_IP=CURLOPTTYPE_LONG+137,
    CURLOPT_FTP_FILEMETHOD=CURLOPTTYPE_LONG+138,
    CURLOPT_LOCALPORT=CURLOPTTYPE_LONG+139,
    CURLOPT_LOCALPORTRANGE=CURLOPTTYPE_LONG+140,
    CURLOPT_CONNECT_ONLY=CURLOPTTYPE_LONG+141,
    CURLOPT_CONV_FROM_NETWORK_FUNCTION=CURLOPTTYPE_FUNCTIONPOINT+142,
    CURLOPT_CONV_TO_NETWORK_FUNCTION=CURLOPTTYPE_FUNCTIONPOINT+143,
    CURLOPT_CONV_FROM_UTF8_FUNCTION=CURLOPTTYPE_FUNCTIONPOINT+144,
    CURLOPT_MAX_SEND_SPEED_LARGE=CURLOPTTYPE_OFF_T+145,
    CURLOPT_MAX_RECV_SPEED_LARGE=CURLOPTTYPE_OFF_T+146,
    CURLOPT_FTP_ALTERNATIVE_TO_USER=CURLOPTTYPE_OBJECTPOINT+147,
    CURLOPT_SOCKOPTFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+148,
    CURLOPT_SOCKOPTDATA=CURLOPTTYPE_OBJECTPOINT+149,
    CURLOPT_SSL_SESSIONID_CACHE=CURLOPTTYPE_LONG+150,
    CURLOPT_SSH_AUTH_TYPES=CURLOPTTYPE_LONG+151,
    CURLOPT_SSH_PUBLIC_KEYFILE=CURLOPTTYPE_OBJECTPOINT+152,
    CURLOPT_SSH_PRIVATE_KEYFILE=CURLOPTTYPE_OBJECTPOINT+153,
    CURLOPT_FTP_SSL_CCC=CURLOPTTYPE_LONG+154,
    CURLOPT_TIMEOUT_MS=CURLOPTTYPE_LONG+155,
    CURLOPT_CONNECTTIMEOUT_MS=CURLOPTTYPE_LONG+156,
    CURLOPT_HTTP_TRANSFER_DECODING=CURLOPTTYPE_LONG+157,
    CURLOPT_HTTP_CONTENT_DECODING=CURLOPTTYPE_LONG+158,
    CURLOPT_NEW_FILE_PERMS=CURLOPTTYPE_LONG+159,
    CURLOPT_NEW_DIRECTORY_PERMS=CURLOPTTYPE_LONG+160,
    CURLOPT_POSTREDIR=CURLOPTTYPE_LONG+161,
    CURLOPT_SSH_HOST_PUBLIC_KEY_MD5=CURLOPTTYPE_OBJECTPOINT+162,
    CURLOPT_OPENSOCKETFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+163,
    CURLOPT_OPENSOCKETDATA=CURLOPTTYPE_OBJECTPOINT+164,
    CURLOPT_COPYPOSTFIELDS=CURLOPTTYPE_OBJECTPOINT+165,
    CURLOPT_PROXY_TRANSFER_MODE=CURLOPTTYPE_LONG+166,
    CURLOPT_SEEKFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+167,
    CURLOPT_SEEKDATA=CURLOPTTYPE_OBJECTPOINT+168,
    CURLOPT_CRLFILE=CURLOPTTYPE_OBJECTPOINT+169,
    CURLOPT_ISSUERCERT=CURLOPTTYPE_OBJECTPOINT+170,
    CURLOPT_ADDRESS_SCOPE=CURLOPTTYPE_LONG+171,
    CURLOPT_CERTINFO=CURLOPTTYPE_LONG+172,
    CURLOPT_USERNAME=CURLOPTTYPE_OBJECTPOINT+173,
    CURLOPT_PASSWORD=CURLOPTTYPE_OBJECTPOINT+174,
    CURLOPT_PROXYUSERNAME=CURLOPTTYPE_OBJECTPOINT+175,
    CURLOPT_PROXYPASSWORD=CURLOPTTYPE_OBJECTPOINT+176,
    CURLOPT_NOPROXY=CURLOPTTYPE_OBJECTPOINT+177,
    CURLOPT_TFTP_BLKSIZE=CURLOPTTYPE_LONG+178,
    CURLOPT_SOCKS5_GSSAPI_SERVICE=CURLOPTTYPE_OBJECTPOINT+179,
    CURLOPT_SOCKS5_GSSAPI_NEC=CURLOPTTYPE_LONG+180,
    CURLOPT_PROTOCOLS=CURLOPTTYPE_LONG+181,
    CURLOPT_REDIR_PROTOCOLS=CURLOPTTYPE_LONG+182,
    CURLOPT_SSH_KNOWNHOSTS=CURLOPTTYPE_OBJECTPOINT+183,
    CURLOPT_SSH_KEYFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+184,
    CURLOPT_SSH_KEYDATA=CURLOPTTYPE_OBJECTPOINT+185,
    CURLOPT_MAIL_FROM=CURLOPTTYPE_OBJECTPOINT+186,
    CURLOPT_MAIL_RCPT=CURLOPTTYPE_OBJECTPOINT+187,
    CURLOPT_FTP_USE_PRET=CURLOPTTYPE_LONG+188,
    CURLOPT_RTSP_REQUEST=CURLOPTTYPE_LONG+189,
    CURLOPT_RTSP_SESSION_ID=CURLOPTTYPE_OBJECTPOINT+190,
    CURLOPT_RTSP_STREAM_URI=CURLOPTTYPE_OBJECTPOINT+191,
    CURLOPT_RTSP_TRANSPORT=CURLOPTTYPE_OBJECTPOINT+192,
    CURLOPT_RTSP_CLIENT_CSEQ=CURLOPTTYPE_LONG+193,
    CURLOPT_RTSP_SERVER_CSEQ=CURLOPTTYPE_LONG+194,
    CURLOPT_INTERLEAVEDATA=CURLOPTTYPE_OBJECTPOINT+195,
    CURLOPT_INTERLEAVEFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+196,
    CURLOPT_WILDCARDMATCH=CURLOPTTYPE_LONG+197,
    CURLOPT_CHUNK_BGN_FUNCTION=CURLOPTTYPE_FUNCTIONPOINT+198,
    CURLOPT_CHUNK_END_FUNCTION=CURLOPTTYPE_FUNCTIONPOINT+199,
    CURLOPT_FNMATCH_FUNCTION=CURLOPTTYPE_FUNCTIONPOINT+200,
    CURLOPT_CHUNK_DATA=CURLOPTTYPE_OBJECTPOINT+201,
    CURLOPT_FNMATCH_DATA=CURLOPTTYPE_OBJECTPOINT+202,
    CURLOPT_RESOLVE=CURLOPTTYPE_OBJECTPOINT+203,
    CURLOPT_TLSAUTH_USERNAME=CURLOPTTYPE_OBJECTPOINT+204,
    CURLOPT_TLSAUTH_PASSWORD=CURLOPTTYPE_OBJECTPOINT+205,
    CURLOPT_TLSAUTH_TYPE=CURLOPTTYPE_OBJECTPOINT+206,
    CURLOPT_TRANSFER_ENCODING=CURLOPTTYPE_LONG+207,
    CURLOPT_CLOSESOCKETFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+208,
    CURLOPT_CLOSESOCKETDATA=CURLOPTTYPE_OBJECTPOINT+209,
    CURLOPT_GSSAPI_DELEGATION=CURLOPTTYPE_LONG+210,
    CURLOPT_DNS_SERVERS=CURLOPTTYPE_OBJECTPOINT+211,
    CURLOPT_ACCEPTTIMEOUT_MS=CURLOPTTYPE_LONG+212,
    CURLOPT_TCP_KEEPALIVE=CURLOPTTYPE_LONG+213,
    CURLOPT_TCP_KEEPIDLE=CURLOPTTYPE_LONG+214,
    CURLOPT_TCP_KEEPINTVL=CURLOPTTYPE_LONG+215,
    CURLOPT_SSL_OPTIONS=CURLOPTTYPE_LONG+216,
    CURLOPT_MAIL_AUTH=CURLOPTTYPE_OBJECTPOINT+217,
    CURLOPT_LASTENTRY
    );
(*
/* These enums are for use with the CURLOPT_HTTP_VERSION option. */
enum {
  CURL_HTTP_VERSION_NONE, /* setting this means we don't care, and that we'd
                             like the library to choose the best possible
                             for us! */
  CURL_HTTP_VERSION_1_0,  /* please use HTTP 1.0 in the request */
  CURL_HTTP_VERSION_1_1,  /* please use HTTP 1.1 in the request */

  CURL_HTTP_VERSION_LAST /* *ILLEGAL* http version */
};
*)
  TCUrlHTTPVersion=(
    CURL_HTTP_VERSION_NONE,
    CURL_HTTP_VERSION_1_0,
    CURL_HTTP_VERSION_1_1,
    CURL_HTTP_VERSION_LAST
  );
(*
/*
 * Public API enums for RTSP requests
 */
enum {
    CURL_RTSPREQ_NONE, /* first in list */
    CURL_RTSPREQ_OPTIONS,
    CURL_RTSPREQ_DESCRIBE,
    CURL_RTSPREQ_ANNOUNCE,
    CURL_RTSPREQ_SETUP,
    CURL_RTSPREQ_PLAY,
    CURL_RTSPREQ_PAUSE,
    CURL_RTSPREQ_TEARDOWN,
    CURL_RTSPREQ_GET_PARAMETER,
    CURL_RTSPREQ_SET_PARAMETER,
    CURL_RTSPREQ_RECORD,
    CURL_RTSPREQ_RECEIVE,
    CURL_RTSPREQ_LAST /* last in list */
};
*)
  TCurlRTSPRequest=(
    CURL_RTSPREQ_NONE,
    CURL_RTSPREQ_OPTIONS,
    CURL_RTSPREQ_DESCRIBE,
    CURL_RTSPREQ_ANNOUNCE,
    CURL_RTSPREQ_SETUP,
    CURL_RTSPREQ_PLAY,
    CURL_RTSPREQ_PAUSE,
    CURL_RTSPREQ_TEARDOWN,
    CURL_RTSPREQ_GET_PARAMETER,
    CURL_RTSPREQ_SET_PARAMETER,
    CURL_RTSPREQ_RECORD,
    CURL_RTSPREQ_RECEIVE,
    CURL_RTSPREQ_LAST
  );

(*/* These enums are for use with the CURLOPT_NETRC option. */
enum CURL_NETRC_OPTION {
  CURL_NETRC_IGNORED,     /* The .netrc will never be read.
                           * This is the default. */
  CURL_NETRC_OPTIONAL,    /* A user:password in the URL will be preferred
                           * to one in the .netrc. */
  CURL_NETRC_REQUIRED,    /* A user:password in the URL will be ignored.
                           * Unless one is set programmatically, the .netrc
                           * will be queried. */
  CURL_NETRC_LAST
};*)
  TCurlNetrcOption=(
    CURL_NETRC_IGNORED,
    CURL_NETRC_OPTIONAL,
    CURL_NETRC_REQUIRED,
    CURL_NETRC_LAST
    );
(*
enum {
  CURL_SSLVERSION_DEFAULT,
  CURL_SSLVERSION_TLSv1,
  CURL_SSLVERSION_SSLv2,
  CURL_SSLVERSION_SSLv3,

  CURL_SSLVERSION_LAST /* never use, keep last */
};
*)
  TCurlSSLVersion=(
    CURL_SSLVERSION_DEFAULT,
    CURL_SSLVERSION_TLSv1,
    CURL_SSLVERSION_SSLv2,
    CURL_SSLVERSION_SSLv3,
    CURL_SSLVERSION_LAST
    );
(*
enum CURL_TLSAUTH {
  CURL_TLSAUTH_NONE,
  CURL_TLSAUTH_SRP,
  CURL_TLSAUTH_LAST /* never use, keep last */
};
*)
  TCurlTLSAuth=(
    CURL_TLSAUTH_NONE,
    CURL_TLSAUTH_SRP,
    CURL_TLSAUTH_LAST
  );
(*
typedef enum {
  CURL_TIMECOND_NONE,

  CURL_TIMECOND_IFMODSINCE,
  CURL_TIMECOND_IFUNMODSINCE,
  CURL_TIMECOND_LASTMOD,

  CURL_TIMECOND_LAST
} curl_TimeCond;

*)
  TCurlTimeCond=(
    CURL_TIMECOND_NONE,
    CURL_TIMECOND_IFMODSINCE,
    CURL_TIMECOND_IFUNMODSINCE,
    CURL_TIMECOND_LASTMOD,
    CURL_TIMECOND_LAST
  );
(*
typedef enum {
  CFINIT(NOTHING),        /********* the first one is unused ************/

  /*  */
  CFINIT(COPYNAME),
  CFINIT(PTRNAME),
  CFINIT(NAMELENGTH),
  CFINIT(COPYCONTENTS),
  CFINIT(PTRCONTENTS),
  CFINIT(CONTENTSLENGTH),
  CFINIT(FILECONTENT),
  CFINIT(ARRAY),
  CFINIT(OBSOLETE),
  CFINIT(FILE),

  CFINIT(BUFFER),
  CFINIT(BUFFERPTR),
  CFINIT(BUFFERLENGTH),

  CFINIT(CONTENTTYPE),
  CFINIT(CONTENTHEADER),
  CFINIT(FILENAME),
  CFINIT(END),
  CFINIT(OBSOLETE2),

  CFINIT(STREAM),

  CURLFORM_LASTENTRY /* the last unused */
} CURLformoption;
*)
  TCurlFormOption=(
    CURLFORM_NOTHING,
    CURLFORM_COPYNAME,
    CURLFORM_PTRNAME,
    CURLFORM_NAMELENGTH,
    CURLFORM_COPYCONTENTS,
    CURLFORM_PTRCONTENTS,
    CURLFORM_CONTENTSLENGTH,
    CURLFORM_FILECONTENT,
    CURLFORM_ARRAY,
    CURLFORM_OBSOLETE,
    CURLFORM_FILE,
    CURLFORM_BUFFER,
    CURLFORM_BUFFERPTR,
    CURLFORM_BUFFERLENGTH,
    CURLFORM_CONTENTTYPE,
    CURLFORM_CONTENTHEADER,
    CURLFORM_FILENAME,
    CURLFORM_END,
    CURLFORM_OBSOLETE2,
    CURLFORM_STREAM
    );
(*/* structure to be used as parameter for CURLFORM_ARRAY */
struct curl_forms {
  CURLformoption option;
  const char     *value;
};
*)
  TCurlForms=record
    option:TCurlFormOption;
    value:PAnsiChar;
  end;

(*
/*
 * callback function for curl_formget()
 * The void *arg pointer will be the one passed as second argument to
 *   curl_formget().
 * The character buffer passed to it must not be freed.
 * Should return the buffer length passed to it as the argument "len" on
 *   success.
 */
typedef size_t (*curl_formget_callback)(void *arg, const char *buf,
                                        size_t len);
*)
  TCurrFormGetCallback=function(arg:Pointer;const buf:PAnsiChar;len:Integer):Integer;cdecl;
(*
/* info about the certificate chain, only for OpenSSL builds. Asked
   for with CURLOPT_CERTINFO / CURLINFO_CERTINFO */
struct curl_certinfo {
  int num_of_certs;             /* number of certificates with information */
  struct curl_slist **certinfo; /* for each index in this array, there's a
                                   linked list with textual information in the
                                   format "name: value" */
};
*)
  PCurlCertInfo=^TCurlCertInfo;
  PPCurlCertInfo=^PCurlCertInfo;
  TCurlCertInfo=record
    num_of_certs:Integer;
    certinfo:PPCurlCertInfo;
  end;

(*
typedef enum {
  CURLINFO_NONE, /* first, never use this */
  CURLINFO_EFFECTIVE_URL    = CURLINFO_STRING + 1,
  CURLINFO_RESPONSE_CODE    = CURLINFO_LONG   + 2,
  CURLINFO_TOTAL_TIME       = CURLINFO_DOUBLE + 3,
  CURLINFO_NAMELOOKUP_TIME  = CURLINFO_DOUBLE + 4,
  CURLINFO_CONNECT_TIME     = CURLINFO_DOUBLE + 5,
  CURLINFO_PRETRANSFER_TIME = CURLINFO_DOUBLE + 6,
  CURLINFO_SIZE_UPLOAD      = CURLINFO_DOUBLE + 7,
  CURLINFO_SIZE_DOWNLOAD    = CURLINFO_DOUBLE + 8,
  CURLINFO_SPEED_DOWNLOAD   = CURLINFO_DOUBLE + 9,
  CURLINFO_SPEED_UPLOAD     = CURLINFO_DOUBLE + 10,
  CURLINFO_HEADER_SIZE      = CURLINFO_LONG   + 11,
  CURLINFO_REQUEST_SIZE     = CURLINFO_LONG   + 12,
  CURLINFO_SSL_VERIFYRESULT = CURLINFO_LONG   + 13,
  CURLINFO_FILETIME         = CURLINFO_LONG   + 14,
  CURLINFO_CONTENT_LENGTH_DOWNLOAD   = CURLINFO_DOUBLE + 15,
  CURLINFO_CONTENT_LENGTH_UPLOAD     = CURLINFO_DOUBLE + 16,
  CURLINFO_STARTTRANSFER_TIME = CURLINFO_DOUBLE + 17,
  CURLINFO_CONTENT_TYPE     = CURLINFO_STRING + 18,
  CURLINFO_REDIRECT_TIME    = CURLINFO_DOUBLE + 19,
  CURLINFO_REDIRECT_COUNT   = CURLINFO_LONG   + 20,
  CURLINFO_PRIVATE          = CURLINFO_STRING + 21,
  CURLINFO_HTTP_CONNECTCODE = CURLINFO_LONG   + 22,
  CURLINFO_HTTPAUTH_AVAIL   = CURLINFO_LONG   + 23,
  CURLINFO_PROXYAUTH_AVAIL  = CURLINFO_LONG   + 24,
  CURLINFO_OS_ERRNO         = CURLINFO_LONG   + 25,
  CURLINFO_NUM_CONNECTS     = CURLINFO_LONG   + 26,
  CURLINFO_SSL_ENGINES      = CURLINFO_SLIST  + 27,
  CURLINFO_COOKIELIST       = CURLINFO_SLIST  + 28,
  CURLINFO_LASTSOCKET       = CURLINFO_LONG   + 29,
  CURLINFO_FTP_ENTRY_PATH   = CURLINFO_STRING + 30,
  CURLINFO_REDIRECT_URL     = CURLINFO_STRING + 31,
  CURLINFO_PRIMARY_IP       = CURLINFO_STRING + 32,
  CURLINFO_APPCONNECT_TIME  = CURLINFO_DOUBLE + 33,
  CURLINFO_CERTINFO         = CURLINFO_SLIST  + 34,
  CURLINFO_CONDITION_UNMET  = CURLINFO_LONG   + 35,
  CURLINFO_RTSP_SESSION_ID  = CURLINFO_STRING + 36,
  CURLINFO_RTSP_CLIENT_CSEQ = CURLINFO_LONG   + 37,
  CURLINFO_RTSP_SERVER_CSEQ = CURLINFO_LONG   + 38,
  CURLINFO_RTSP_CSEQ_RECV   = CURLINFO_LONG   + 39,
  CURLINFO_PRIMARY_PORT     = CURLINFO_LONG   + 40,
  CURLINFO_LOCAL_IP         = CURLINFO_STRING + 41,
  CURLINFO_LOCAL_PORT       = CURLINFO_LONG   + 42,
  /* Fill in new entries below here! */

  CURLINFO_LASTONE          = 42
} CURLINFO;
*)
  TCurlInfo=(
  CURLINFO_NONE, 
  CURLINFO_EFFECTIVE_URL    = CURLINFO_STRING + 1,
  CURLINFO_RESPONSE_CODE    = CURLINFO_LONG   + 2,
  CURLINFO_TOTAL_TIME       = CURLINFO_DOUBLE + 3,
  CURLINFO_NAMELOOKUP_TIME  = CURLINFO_DOUBLE + 4,
  CURLINFO_CONNECT_TIME     = CURLINFO_DOUBLE + 5,
  CURLINFO_PRETRANSFER_TIME = CURLINFO_DOUBLE + 6,
  CURLINFO_SIZE_UPLOAD      = CURLINFO_DOUBLE + 7,
  CURLINFO_SIZE_DOWNLOAD    = CURLINFO_DOUBLE + 8,
  CURLINFO_SPEED_DOWNLOAD   = CURLINFO_DOUBLE + 9,
  CURLINFO_SPEED_UPLOAD     = CURLINFO_DOUBLE + 10,
  CURLINFO_HEADER_SIZE      = CURLINFO_LONG   + 11,
  CURLINFO_REQUEST_SIZE     = CURLINFO_LONG   + 12,
  CURLINFO_SSL_VERIFYRESULT = CURLINFO_LONG   + 13,
  CURLINFO_FILETIME         = CURLINFO_LONG   + 14,
  CURLINFO_CONTENT_LENGTH_DOWNLOAD   = CURLINFO_DOUBLE + 15,
  CURLINFO_CONTENT_LENGTH_UPLOAD     = CURLINFO_DOUBLE + 16,
  CURLINFO_STARTTRANSFER_TIME = CURLINFO_DOUBLE + 17,
  CURLINFO_CONTENT_TYPE     = CURLINFO_STRING + 18,
  CURLINFO_REDIRECT_TIME    = CURLINFO_DOUBLE + 19,
  CURLINFO_REDIRECT_COUNT   = CURLINFO_LONG   + 20,
  CURLINFO_PRIVATE          = CURLINFO_STRING + 21,
  CURLINFO_HTTP_CONNECTCODE = CURLINFO_LONG   + 22,
  CURLINFO_HTTPAUTH_AVAIL   = CURLINFO_LONG   + 23,
  CURLINFO_PROXYAUTH_AVAIL  = CURLINFO_LONG   + 24,
  CURLINFO_OS_ERRNO         = CURLINFO_LONG   + 25,
  CURLINFO_NUM_CONNECTS     = CURLINFO_LONG   + 26,
  CURLINFO_SSL_ENGINES      = CURLINFO_SLIST  + 27,
  CURLINFO_COOKIELIST       = CURLINFO_SLIST  + 28,
  CURLINFO_LASTSOCKET       = CURLINFO_LONG   + 29,
  CURLINFO_FTP_ENTRY_PATH   = CURLINFO_STRING + 30,
  CURLINFO_REDIRECT_URL     = CURLINFO_STRING + 31,
  CURLINFO_PRIMARY_IP       = CURLINFO_STRING + 32,
  CURLINFO_APPCONNECT_TIME  = CURLINFO_DOUBLE + 33,
  CURLINFO_CERTINFO         = CURLINFO_SLIST  + 34,
  CURLINFO_CONDITION_UNMET  = CURLINFO_LONG   + 35,
  CURLINFO_RTSP_SESSION_ID  = CURLINFO_STRING + 36,
  CURLINFO_RTSP_CLIENT_CSEQ = CURLINFO_LONG   + 37,
  CURLINFO_RTSP_SERVER_CSEQ = CURLINFO_LONG   + 38,
  CURLINFO_RTSP_CSEQ_RECV   = CURLINFO_LONG   + 39,
  CURLINFO_PRIMARY_PORT     = CURLINFO_LONG   + 40,
  CURLINFO_LOCAL_IP         = CURLINFO_STRING + 41,
  CURLINFO_LOCAL_PORT       = CURLINFO_LONG   + 42,
  CURLINFO_LASTONE          = 42
  );
(*
typedef enum {
  CURLCLOSEPOLICY_NONE, /* first, never use this */

  CURLCLOSEPOLICY_OLDEST,
  CURLCLOSEPOLICY_LEAST_RECENTLY_USED,
  CURLCLOSEPOLICY_LEAST_TRAFFIC,
  CURLCLOSEPOLICY_SLOWEST,
  CURLCLOSEPOLICY_CALLBACK,

  CURLCLOSEPOLICY_LAST /* last, never use this */
} curl_closepolicy;

*)
  TCurlClosePolicy=(
    CURLCLOSEPOLICY_NONE,
    CURLCLOSEPOLICY_OLDEST,
    CURLCLOSEPOLICY_LEAST_RECENTLY_USED,
    CURLCLOSEPOLICY_LEAST_TRAFFIC,
    CURLCLOSEPOLICY_SLOWEST,
    CURLCLOSEPOLICY_CALLBACK,
    CURLCLOSEPOLICY_LAST
  );
(*
/*****************************************************************************
 * Setup defines, protos etc for the sharing stuff.
 */

/* Different data locks for a single share */
typedef enum {
  CURL_LOCK_DATA_NONE = 0,
  /*  CURL_LOCK_DATA_SHARE is used internally to say that
   *  the locking is just made to change the internal state of the share
   *  itself.
   */
  CURL_LOCK_DATA_SHARE,
  CURL_LOCK_DATA_COOKIE,
  CURL_LOCK_DATA_DNS,
  CURL_LOCK_DATA_SSL_SESSION,
  CURL_LOCK_DATA_CONNECT,
  CURL_LOCK_DATA_LAST
} curl_lock_data;
*)
  TCurlLockData=(
    CURL_LOCK_DATA_NONE = 0,
    CURL_LOCK_DATA_SHARE,
    CURL_LOCK_DATA_COOKIE,
    CURL_LOCK_DATA_DNS,
    CURL_LOCK_DATA_SSL_SESSION,
    CURL_LOCK_DATA_CONNECT,
    CURL_LOCK_DATA_LAST
    );
(*
typedef enum {
  CURL_LOCK_ACCESS_NONE = 0,   /* unspecified action */
  CURL_LOCK_ACCESS_SHARED = 1, /* for read perhaps */
  CURL_LOCK_ACCESS_SINGLE = 2, /* for write perhaps */
  CURL_LOCK_ACCESS_LAST        /* never use */
} curl_lock_access;
*)
  TCurlLockAccess=(
    CURL_LOCK_ACCESS_NONE = 0,
    CURL_LOCK_ACCESS_SHARED = 1,
    CURL_LOCK_ACCESS_SINGLE = 2,
    CURL_LOCK_ACCESS_LAST
    );
(*
typedef void (*curl_lock_function)(CURL *handle,
                                   curl_lock_data data,
                                   curl_lock_access locktype,
                                   void *userptr);
*)
  TCurlLockFunction=procedure (handle:THandle;data:TCurlLockData;locktype:TCurlLockAccess;userptr:Pointer);cdecl;
(*
typedef void (*curl_unlock_function)(CURL *handle,
                                     curl_lock_data data,
                                     void *userptr);
*)
  TCurlUnlockFunction=procedure(handle:THandle;data:TCurlLockData;userptr:Pointer);cdecl;
(*
typedef void CURLSH;

typedef enum {
  CURLSHE_OK,  /* all is fine */
  CURLSHE_BAD_OPTION, /* 1 */
  CURLSHE_IN_USE,     /* 2 */
  CURLSHE_INVALID,    /* 3 */
  CURLSHE_NOMEM,      /* 4 out of memory */
  CURLSHE_NOT_BUILT_IN, /* 5 feature not present in lib */
  CURLSHE_LAST        /* never use */
} CURLSHcode;
*)
  TCurlSHCode=(
    CURLSHE_OK,
    CURLSHE_BAD_OPTION,
    CURLSHE_IN_USE,
    CURLSHE_INVALID,
    CURLSHE_NOMEM,
    CURLSHE_NOT_BUILT_IN,
    CURLSHE_LAST
  );
(*
typedef enum {
  CURLSHOPT_NONE,  /* don't use */
  CURLSHOPT_SHARE,   /* specify a data type to share */
  CURLSHOPT_UNSHARE, /* specify which data type to stop sharing */
  CURLSHOPT_LOCKFUNC,   /* pass in a 'curl_lock_function' pointer */
  CURLSHOPT_UNLOCKFUNC, /* pass in a 'curl_unlock_function' pointer */
  CURLSHOPT_USERDATA,   /* pass in a user data pointer used in the lock/unlock
                           callback functions */
  CURLSHOPT_LAST  /* never use */
} CURLSHoption;
*)
  TCurlSHOption=(
    CURLSHOPT_NONE,
    CURLSHOPT_SHARE,
    CURLSHOPT_UNSHARE,
    CURLSHOPT_LOCKFUNC,
    CURLSHOPT_UNLOCKFUNC,
    CURLSHOPT_USERDATA,
    CURLSHOPT_LAST
    );
(*
/****************************************************************************
 * Structures for querying information about the curl library at runtime.
 */

typedef enum {
  CURLVERSION_FIRST,
  CURLVERSION_SECOND,
  CURLVERSION_THIRD,
  CURLVERSION_FOURTH,
  CURLVERSION_LAST /* never actually use this */
} CURLversion;

*)
  TCurlVersion=(
    CURLVERSION_FIRST,
    CURLVERSION_SECOND,
    CURLVERSION_THIRD,
    CURLVERSION_FOURTH,
    CURLVERSION_LAST
  );
(*
typedef struct {
  CURLversion age;          /* age of the returned struct */
  const char *version;      /* LIBCURL_VERSION */
  unsigned int version_num; /* LIBCURL_VERSION_NUM */
  const char *host;         /* OS/host/cpu/machine when configured */
  int features;             /* bitmask, see defines below */
  const char *ssl_version;  /* human readable string */
  long ssl_version_num;     /* not used anymore, always 0 */
  const char *libz_version; /* human readable string */
  /* protocols is terminated by an entry with a NULL protoname */
  const char * const *protocols;

  /* The fields below this were added in CURLVERSION_SECOND */
  const char *ares;
  int ares_num;

  /* This field was added in CURLVERSION_THIRD */
  const char *libidn;

  /* These field were added in CURLVERSION_FOURTH */

  /* Same as '_libiconv_version' if built with HAVE_ICONV */
  int iconv_ver_num;

  const char *libssh_version; /* human readable string */

} curl_version_info_data;
*)
  TCurlVersionInfoData=record
    age:TCURLversion;         
    version:PAnsiChar;
    version_num:Cardinal;
    host:PAnsiChar;
    features:Integer;
    ssl_version:PAnsiChar;
    ssl_version_num:longint;
    libz_version:PAnsiChar;
    protocols:PPAnsiChar;
    ares:PAnsichar;
    ares_num:Integer;
    libidn:PAnsiChar;
    iconv_ver_num:Integer;
    libssh_version:PAnsiChar;
  end;
//以下改编自multi.h
(*  typedef enum {
  CURLM_CALL_MULTI_PERFORM = -1, /* please call curl_multi_perform() or
                                    curl_multi_socket*() soon */
  CURLM_OK,
  CURLM_BAD_HANDLE,      /* the passed-in handle is not a valid CURLM handle */
  CURLM_BAD_EASY_HANDLE, /* an easy handle was not good/valid */
  CURLM_OUT_OF_MEMORY,   /* if you ever get this, you're in deep sh*t */
  CURLM_INTERNAL_ERROR,  /* this is a libcurl bug */
  CURLM_BAD_SOCKET,      /* the passed in socket argument did not match */
  CURLM_UNKNOWN_OPTION,  /* curl_multi_setopt() with unsupported option */
  CURLM_LAST
} CURLMcode;*)

  TCUrlMCode=(
    CURLM_CALL_MULTI_PERFORM = -1,
    CURLM_OK,
    CURLM_BAD_HANDLE,
    CURLM_BAD_EASY_HANDLE,
    CURLM_OUT_OF_MEMORY,
    CURLM_INTERNAL_ERROR,
    CURLM_BAD_SOCKET,
    CURLM_UNKNOWN_OPTION,
    CURLM_LAST
  );
(*
typedef enum {
  CURLMSG_NONE, /* first, not used */
  CURLMSG_DONE, /* This easy handle has completed. 'result' contains
                   the CURLcode of the transfer */
  CURLMSG_LAST /* last, not used */
} CURLMSG;
*)
  TCurlMsgType=(
    CURLMSG_NONE,
    CURLMSG_DONE,
    CURLMSG_LAST
    );
(*
struct CURLMsg {
  CURLMSG msg;       /* what this message means */
  CURL *easy_handle; /* the handle it concerns */
  union {
    void *whatever;    /* message-specific data */
    CURLcode result;   /* return code for transfer */
  } data;
};
*)
  PCurlMsg=^TCurlMsg;
  TCurlMsgData=record
    case Integer of
      0:(whatever:Pointer);
      1:(Result:TCUrlCode);
    end;
  TCurlMsg=record
    msg:TCurlMsgType;
    easy_handle:THandle;
    data:TCurlMsgData;
  end;
(*
struct curl_waitfd {
  curl_socket_t fd;
  short events;
  short revents; /* not supported yet */
};
*)
  TCurlWaitFd=record
    fd:TSocket;
    events:Smallint;
    revents:Smallint;
  end;
(*
typedef int (*curl_socket_callback)(CURL *easy,      /* easy handle */
                                    curl_socket_t s, /* socket */
                                    int what,        /* see above */
                                    void *userp,     /* private callback
                                                        pointer */
                                    void *socketp);  /* private socket
                                                        pointer */
*)
  TCurlSocketCallback=function(easy:THandle;s:TSocket;what:Integer;userp,socketp:Pointer):Integer;cdecl;
(*/*
 * Name:    curl_multi_timer_callback
 *
 * Desc:    Called by libcurl whenever the library detects a change in the
 *          maximum number of milliseconds the app is allowed to wait before
 *          curl_multi_socket() or curl_multi_perform() must be called
 *          (to allow libcurl's timed events to take place).
 *
 * Returns: The callback should return zero.
 */
typedef int (*curl_multi_timer_callback)(CURLM *multi,    /* multi handle */
                                         long timeout_ms, /* see above */
                                         void *userp);    /* private callback
                                                             pointer */
*)
  TCurlMultiTimerCallback=function(multi:THandle;timeout_ms:LongInt;userp:Pointer):Integer;
(*

typedef enum {
  /* This is the socket callback function pointer */
  CINIT(SOCKETFUNCTION, FUNCTIONPOINT, 1),

  /* This is the argument passed to the socket callback */
  CINIT(SOCKETDATA, OBJECTPOINT, 2),

    /* set to 1 to enable pipelining for this multi handle */
  CINIT(PIPELINING, LONG, 3),

   /* This is the timer callback function pointer */
  CINIT(TIMERFUNCTION, FUNCTIONPOINT, 4),

  /* This is the argument passed to the timer callback */
  CINIT(TIMERDATA, OBJECTPOINT, 5),

  /* maximum number of entries in the connection cache */
  CINIT(MAXCONNECTS, LONG, 6),

  /* maximum number of (pipelining) connections to one host */
  CINIT(MAX_HOST_CONNECTIONS, LONG, 7),

  /* maximum number of requests in a pipeline */
  CINIT(MAX_PIPELINE_LENGTH, LONG, 8),

  /* a connection with a content-length longer than this
     will not be considered for pipelining */
  CINIT(CONTENT_LENGTH_PENALTY_SIZE, OFF_T, 9),

  /* a connection with a chunk length longer than this
     will not be considered for pipelining */
  CINIT(CHUNK_LENGTH_PENALTY_SIZE, OFF_T, 10),

  /* a list of site names(+port) that are blacklisted from
     pipelining */
  CINIT(PIPELINING_SITE_BL, OBJECTPOINT, 11),

  /* a list of server types that are blacklisted from
     pipelining */
  CINIT(PIPELINING_SERVER_BL, OBJECTPOINT, 12),

  /* maximum number of open connections in total */
  CINIT(MAX_TOTAL_CONNECTIONS, LONG, 13),

  CURLMOPT_LASTENTRY /* the last unused */
} CURLMoption;

*)
  TCurlMOption=(
    CURLMOPT_SOCKETFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+1,
    CURLMOPT_SOCKETDATA=CURLOPTTYPE_OBJECTPOINT+2,
    CURLMOPT_PIPELINING=CURLOPTTYPE_LONG+3,
    CURLMOPT_TIMERFUNCTION=CURLOPTTYPE_FUNCTIONPOINT+4,
    CURLMOPT_TIMERDATA=CURLOPTTYPE_OBJECTPOINT+5,
    CURLMOPT_MAXCONNECTS=CURLOPTTYPE_LONG+6,
    CURLMOPT_MAX_HOST_CONNECTIONS=CURLOPTTYPE_LONG+7,
    CURLMOPT_MAX_PIPELINE_LENGTH=CURLOPTTYPE_LONG+8,
    CURLMOPT_CONTENT_LENGTH_PENALTY_SIZE=CURLOPTTYPE_OFF_T+9,
    CURLMOPT_CHUNK_LENGTH_PENALTY_SIZE=CURLOPTTYPE_OFF_T+10,
    CURLMOPT_PIPELINING_SITE_BL=CURLOPTTYPE_OBJECTPOINT+11,
    CURLMOPT_PIPELINING_SERVER_BL=CURLOPTTYPE_OBJECTPOINT+12,
    CURLMOPT_MAX_TOTAL_CONNECTIONS=CURLOPTTYPE_LONG+13
  );

//改编自multi.h的类型声明结束
(*
/* three convenient "aliases" that follow the name scheme better */
#define CURLOPT_WRITEDATA CURLOPT_FILE
#define CURLOPT_READDATA  CURLOPT_INFILE
#define CURLOPT_HEADERDATA CURLOPT_WRITEHEADER
#define CURLOPT_RTSPHEADER CURLOPT_HTTPHEADER
*)
const
  CURLOPT_WRITEDATA=CURLOPT_FILE;
  CURLOPT_READDATA=CURLOPT_INFILE;
  CURLOPT_HEADERDATA=CURLOPT_WRITEHEADER;
  CURLOPT_RTSPHEADER=CURLOPT_HTTPHEADER;
  CURLINFO_HTTP_CODE=CURLINFO_RESPONSE_CODE;
  CURLVERSION_NOW=CURLVERSION_FOURTH;
  //multi.h
  CURLM_CALL_MULTI_SOCKET=CURLM_CALL_MULTI_PERFORM;  
(*/*
 * NAME curl_formadd()
 *
 * DESCRIPTION
 *
 * Pretty advanced function for building multi-part formposts. Each invoke
 * adds one part that together construct a full post. Then use
 * CURLOPT_HTTPPOST to send it off to libcurl.
 */
CURL_EXTERN CURLFORMcode curl_formadd(struct curl_httppost **httppost,
                                      struct curl_httppost **last_post,
                                      ...);*)
function curl_formadd(httppost,lastpost:PPCurlHttpPost):TCUrlFormCode;cdecl varargs;
(*/*
 * NAME curl_formget()
 *
 * DESCRIPTION
 *
 * Serialize a curl_httppost struct built with curl_formadd().
 * Accepts a void pointer as second argument which will be passed to
 * the curl_formget_callback function.
 * Returns 0 on success.
 */
CURL_EXTERN int curl_formget(struct curl_httppost *form, void *arg,
                             curl_formget_callback append);
*)
function curl_formget(var form:TCurlHttpPost;arg:Pointer;append:TCurrFormGetCallback):Integer;cdecl;
(*
/*
 * NAME curl_formfree()
 *
 * DESCRIPTION
 *
 * Free a multipart formpost previously built with curl_formadd().
 */
CURL_EXTERN void curl_formfree(struct curl_httppost *form);
*)
procedure curl_formfree(var form:TCurlHttpPost);cdecl;
(*/*
 * NAME curl_getenv()
 *
 * DESCRIPTION
 *
 * Returns a malloc()'ed string that MUST be curl_free()ed after usage is
 * complete. DEPRECATED - see lib/README.curlx
 */
CURL_EXTERN char *curl_getenv(const char *variable);
*)
function curl_getenv(const variable:PAnsiChar):PAnsiChar;cdecl;
(*
/*
 * NAME curl_version()
 *
 * DESCRIPTION
 *
 * Returns a static ascii string of the libcurl version.
 */
CURL_EXTERN char *curl_version(void);
*)
function curl_version:PAnsiChar;cdecl;
(*
/*
 * NAME curl_easy_escape()
 *
 * DESCRIPTION
 *
 * Escapes URL strings (converts all letters consider illegal in URLs to their
 * %XX versions). This function returns a new allocated string or NULL if an
 * error occurred.
 */
CURL_EXTERN char *curl_easy_escape(CURL *handle,
                                   const char *string,
                                   int length);
*)
function curl_easy_escape(handle:THandle;const str:PAnsiChar;length:Integer):PAnsiChar;cdecl;
(*
/* the previous version: */
CURL_EXTERN char *curl_escape(const char *string,
                              int length);
*)
function curl_escape(const str:PAnsiChar;length:Integer):PAnsiChar;cdecl;
(*
/*
 * NAME curl_easy_unescape()
 *
 * DESCRIPTION
 *
 * Unescapes URL encoding in strings (converts all %XX codes to their 8bit
 * versions). This function returns a new allocated string or NULL if an error
 * occurred.
 * Conversion Note: On non-ASCII platforms the ASCII %XX codes are
 * converted into the host encoding.
 */
CURL_EXTERN char *curl_easy_unescape(CURL *handle,
                                     const char *string,
                                     int length,
                                     int *outlength);
*)
function curl_easy_unescape(handle:THandle;const str:PAnsiChar;length:Integer;var outlength:Integer):PAnsiChar;cdecl;
(*
/* the previous version */
CURL_EXTERN char *curl_unescape(const char *string,
                                int length);
*)
function curl_unescape(const str:PAnsiChar;length:Integer):PAnsiChar;cdecl;
(*
/*
 * NAME curl_free()
 *
 * DESCRIPTION
 *
 * Provided for de-allocation in the same translation unit that did the
 * allocation. Added in libcurl 7.10
 */
CURL_EXTERN void curl_free(void *p);
*)
procedure curl_free(p:Pointer);cdecl;
(*
/*
 * NAME curl_global_init()
 *
 * DESCRIPTION
 *
 * curl_global_init() should be invoked exactly once for each application that
 * uses libcurl and before any call of other libcurl functions.
 *
 * This function is not thread-safe!
 */
CURL_EXTERN CURLcode curl_global_init(long flags);
*)
function curl_global_init(flags:Longint):TCurlCode;cdecl;
(*
/*
 * NAME curl_global_init_mem()
 *
 * DESCRIPTION
 *
 * curl_global_init() or curl_global_init_mem() should be invoked exactly once
 * for each application that uses libcurl.  This function can be used to
 * initialize libcurl and set user defined memory management callback
 * functions.  Users can implement memory management routines to check for
 * memory leaks, check for mis-use of the curl library etc.  User registered
 * callback routines with be invoked by this library instead of the system
 * memory management routines like malloc, free etc.
 */
CURL_EXTERN CURLcode curl_global_init_mem(long flags,
                                          curl_malloc_callback m,
                                          curl_free_callback f,
                                          curl_realloc_callback r,
                                          curl_strdup_callback s,
                                          curl_calloc_callback c);
*)
function curl_global_init_mem(flags:Longint;m:TCUrlMallocCallback;f:TCurlFreeCallback;r:TCurlReallocCallback;s:TCurlStrdupCallback;c:TCurlCallocCallback):TCurlCode;cdecl;
(*/*
 * NAME curl_global_cleanup()
 *
 * DESCRIPTION
 *
 * curl_global_cleanup() should be invoked exactly once for each application
 * that uses libcurl
 */
CURL_EXTERN void curl_global_cleanup(void);*)
procedure curl_global_cleanup;cdecl;
(*/*
 * NAME curl_slist_append()
 *
 * DESCRIPTION
 *
 * Appends a string to a linked list. If no list exists, it will be created
 * first. Returns the new list, after appending.
 */
CURL_EXTERN struct curl_slist *curl_slist_append(struct curl_slist *,
                                                 const char *s);
*)
function curl_slist_append(list:PCurlSList;const s:PAnsiChar):PCurlSList;cdecl;
(*
/*
 * NAME curl_slist_free_all()
 *
 * DESCRIPTION
 *
 * free a previously built curl_slist.
 */
CURL_EXTERN void curl_slist_free_all(struct curl_slist *list);
*)
procedure curl_slist_free_all(list:PCurlSList);cdecl;
(*
/*
 * NAME curl_getdate()
 *
 * DESCRIPTION
 *
 * Returns the time, in seconds since 1 Jan 1970 of the time string given in
 * the first argument. The time argument in the second parameter is unused
 * and should be set to NULL.
 */
CURL_EXTERN time_t curl_getdate(const char *p, const time_t *unused);*)
function curl_getdate(const p:PAnsiChar;var unused:longint):longint;cdecl;
(*CURL_EXTERN CURLSH *curl_share_init(void);*)
function curl_share_init:Pointer;cdecl;
(*CURL_EXTERN CURLSHcode curl_share_setopt(CURLSH *, CURLSHoption option, ...);*)
function curl_share_setopt(sh:Pointer;option:TCurlSHOption):TCurlSHCode;cdecl varargs;
(*
CURL_EXTERN CURLSHcode curl_share_cleanup(CURLSH *sh);*)
function curl_share_cleanup(sh:Pointer):TCURLSHCode;cdecl;
(*
/*
 * NAME curl_version_info()
 *
 * DESCRIPTION
 *
 * This function returns a pointer to a static copy of the version info
 * struct. See above.
 */
CURL_EXTERN curl_version_info_data *curl_version_info(CURLversion);

*)
function curl_version_info(ver:TCurlVersion):TCurlVersionInfoData;cdecl;
(*
/*
 * NAME curl_easy_strerror()
 *
 * DESCRIPTION
 *
 * The curl_easy_strerror function may be used to turn a CURLcode value
 * into the equivalent human readable error string.  This is useful
 * for printing meaningful error messages.
 */
CURL_EXTERN const char *curl_easy_strerror(CURLcode);
*)
function curl_easy_strerror(code:TCurlCode):PAnsiChar;cdecl;
(*
/*
 * NAME curl_share_strerror()
 *
 * DESCRIPTION
 *
 * The curl_share_strerror function may be used to turn a CURLSHcode value
 * into the equivalent human readable error string.  This is useful
 * for printing meaningful error messages.
 */
CURL_EXTERN const char *curl_share_strerror(CURLSHcode);
*)
function curl_share_strerror(ACode:TCurlShCode):PAnsiChar;cdecl;
(*
/*
 * NAME curl_easy_pause()
 *
 * DESCRIPTION
 *
 * The curl_easy_pause function pauses or unpauses transfers. Select the new
 * state by setting the bitmask, use the convenience defines below.
 *
 */
CURL_EXTERN CURLcode curl_easy_pause(CURL *handle, int bitmask);
*)
function curl_easy_pause(handle:THandle;bitmask:Integer):TCurlCode;cdecl;

//multi.h
(*/*
 * Name:    curl_multi_init()
 *
 * Desc:    inititalize multi-style curl usage
 *
 * Returns: a new CURLM handle to use in all 'curl_multi' functions.
 */
CURL_EXTERN CURLM *curl_multi_init(void);
*)
function curl_multi_init:THandle;cdecl;
(*
/*
 * Name:    curl_multi_add_handle()
 *
 * Desc:    add a standard curl handle to the multi stack
 *
 * Returns: CURLMcode type, general multi error code.
 */
CURL_EXTERN CURLMcode curl_multi_add_handle(CURLM *multi_handle,
                                            CURL *curl_handle);
*)
function curl_multi_add_handle(multi_handle:THandle;curl_handle:THandle):TCurlMCode;cdecl;
(*
/*
  * Name:    curl_multi_remove_handle()
  *
  * Desc:    removes a curl handle from the multi stack again
  *
  * Returns: CURLMcode type, general multi error code.
  */
CURL_EXTERN CURLMcode curl_multi_remove_handle(CURLM *multi_handle,
                                               CURL *curl_handle);
*)
function curl_multi_remove_handle(multi_handle:THandle;curl_handle:THandle):TCurlMCode;cdecl;
(*
/*
  * Name:    curl_multi_fdset()
  *
  * Desc:    Ask curl for its fd_set sets. The app can use these to select() or
  *          poll() on. We want curl_multi_perform() called as soon as one of
  *          them are ready.
  *
  * Returns: CURLMcode type, general multi error code.
  */
CURL_EXTERN CURLMcode curl_multi_fdset(CURLM *multi_handle,
                                       fd_set *read_fd_set,
                                       fd_set *write_fd_set,
                                       fd_set *exc_fd_set,
                                       int *max_fd);
*)
function curl_multi_fdset(multi_handle:THandle;var read_fd_set,write_fd_set,exc_fd_set:Tfdset;var max_fd:Integer):TCurlMCode;cdecl;
(*
/*
 * Name:     curl_multi_wait()
 *
 * Desc:     Poll on all fds within a CURLM set as well as any
 *           additional fds passed to the function.
 *
 * Returns:  CURLMcode type, general multi error code.
 */
CURL_EXTERN CURLMcode curl_multi_wait(CURLM *multi_handle,
                                      struct curl_waitfd extra_fds[],
                                      unsigned int extra_nfds,
                                      int timeout_ms,
                                      int *ret);
*)
function curl_multi_wait(multi_handle:THandle;var extra_fds:TCurlWaitFd;extra_nfds:Cardinal;timeout_ms:Integer;var ret:Integer):TCurlMCode;cdecl;
(*
/*
  * Name:    curl_multi_perform()
  *
  * Desc:    When the app thinks there's data available for curl it calls this
  *          function to read/write whatever there is right now. This returns
  *          as soon as the reads and writes are done. This function does not
  *          require that there actually is data available for reading or that
  *          data can be written, it can be called just in case. It returns
  *          the number of handles that still transfer data in the second
  *          argument's integer-pointer.
  *
  * Returns: CURLMcode type, general multi error code. *NOTE* that this only
  *          returns errors etc regarding the whole multi stack. There might
  *          still have occurred problems on invidual transfers even when this
  *          returns OK.
  */
CURL_EXTERN CURLMcode curl_multi_perform(CURLM *multi_handle,
                                         int *running_handles);
*)
function curl_multi_perform(multi_handle:THandle;var running_handles:Integer):TCurlMCode;cdecl;
(*
/*
  * Name:    curl_multi_cleanup()
  *
  * Desc:    Cleans up and removes a whole multi stack. It does not free or
  *          touch any individual easy handles in any way. We need to define
  *          in what state those handles will be if this function is called
  *          in the middle of a transfer.
  *
  * Returns: CURLMcode type, general multi error code.
  */
CURL_EXTERN CURLMcode curl_multi_cleanup(CURLM *multi_handle);
*)
function curl_multi_cleanup(multi_handle:THandle):TCurlMCode;cdecl;
(*
/*
 * Name:    curl_multi_info_read()
 *
 * Desc:    Ask the multi handle if there's any messages/informationals from
 *          the individual transfers. Messages include informationals such as
 *          error code from the transfer or just the fact that a transfer is
 *          completed. More details on these should be written down as well.
 *
 *          Repeated calls to this function will return a new struct each
 *          time, until a special "end of msgs" struct is returned as a signal
 *          that there is no more to get at this point.
 *
 *          The data the returned pointer points to will not survive calling
 *          curl_multi_cleanup().
 *
 *          The 'CURLMsg' struct is meant to be very simple and only contain
 *          very basic informations. If more involved information is wanted,
 *          we will provide the particular "transfer handle" in that struct
 *          and that should/could/would be used in subsequent
 *          curl_easy_getinfo() calls (or similar). The point being that we
 *          must never expose complex structs to applications, as then we'll
 *          undoubtably get backwards compatibility problems in the future.
 *
 * Returns: A pointer to a filled-in struct, or NULL if it failed or ran out
 *          of structs. It also writes the number of messages left in the
 *          queue (after this read) in the integer the second argument points
 *          to.
 */
CURL_EXTERN CURLMsg *curl_multi_info_read(CURLM *multi_handle,
                                          int *msgs_in_queue);
*)
function curl_multi_info_read(multi_handle:THandle;var msgs_in_queue:Integer):PCUrlMsg;cdecl;
(*
/*
 * Name:    curl_multi_strerror()
 *
 * Desc:    The curl_multi_strerror function may be used to turn a CURLMcode
 *          value into the equivalent human readable error string.  This is
 *          useful for printing meaningful error messages.
 *
 * Returns: A pointer to a zero-terminated error message.
 */
CURL_EXTERN const char *curl_multi_strerror(CURLMcode);
*)
function curl_multi_strerror(code:TCurlMCode):PAnsiChar;cdecl;
(*
CURL_EXTERN CURLMcode curl_multi_socket(CURLM *multi_handle, curl_socket_t s,
                                        int *running_handles);

CURL_EXTERN CURLMcode curl_multi_socket_action(CURLM *multi_handle,
                                               curl_socket_t s,
                                               int ev_bitmask,
                                               int *running_handles);

CURL_EXTERN CURLMcode curl_multi_socket_all(CURLM *multi_handle,
                                            int *running_handles);
*)
function curl_multi_socket(multi_handle:THandle;s:TSocket;var running_handles:Integer) :TCurlMCode;cdecl;
function curl_multi_socket_action(multi_handle:THandle;s:TSocket;ev_bitmask:Integer;var running_handles:Integer):TCurlMCode;cdecl;
function curl_multi_socket_all(multi_handle:THandle;var running_handles:Integer):TCurlMCode;cdecl;
(*
/*
 * Name:    curl_multi_timeout()
 *
 * Desc:    Returns the maximum number of milliseconds the app is allowed to
 *          wait before curl_multi_socket() or curl_multi_perform() must be
 *          called (to allow libcurl's timed events to take place).
 *
 * Returns: CURLM error code.
 */
CURL_EXTERN CURLMcode curl_multi_timeout(CURLM *multi_handle,
                                         long *milliseconds);
*)
function curl_multi_timeout(multi_handle:THandle;var milliseconds:LongInt):TCurlMCode;cdecl;
(*
/*
 * Name:    curl_multi_setopt()
 *
 * Desc:    Sets options for the multi handle.
 *
 * Returns: CURLM error code.
 */
CURL_EXTERN CURLMcode curl_multi_setopt(CURLM *multi_handle,
                                        CURLMoption option, ...);
*)
function curl_multi_setopt(multi_handle:THandle;option:TCUrlMOption):TCurlMCode;cdecl varargs;
(*
/*
 * Name:    curl_multi_assign()
 *
 * Desc:    This function sets an association in the multi handle between the
 *          given socket and a private pointer of the application. This is
 *          (only) useful for curl_multi_socket uses.
 *
 * Returns: CURLM error code.
 */
CURL_EXTERN CURLMcode curl_multi_assign(CURLM *multi_handle,
                                        curl_socket_t sockfd, void *sockp);
*)
function curl_multi_assign(multi_handle:THandle;sockfd:TSocket;sockp:Pointer):TCurlMCode;cdecl;

//easy.h
//CURL_EXTERN CURL *curl_easy_init(void);
//CURL_EXTERN CURLcode curl_easy_setopt(CURL *curl, CURLoption option, ...);
//CURL_EXTERN CURLcode curl_easy_perform(CURL *curl);
//CURL_EXTERN void curl_easy_cleanup(CURL *curl);

function curl_easy_init:THandle;cdecl;
function curl_easy_setopt(curl:THandle;option:TCurlOption):TCurlCode;cdecl varargs;
function curl_easy_perform(curl:THandle):TCurlCode;cdecl;
procedure curl_easy_cleanup(curl:THandle);cdecl;
(*/*
 * NAME curl_easy_getinfo()
 *
 * DESCRIPTION
 *
 * Request internal information from the curl session with this function.  The
 * third argument MUST be a pointer to a long, a pointer to a char * or a
 * pointer to a double (as the documentation describes elsewhere).  The data
 * pointed to will be filled in accordingly and can be relied upon only if the
 * function returns CURLE_OK.  This function is intended to get used *AFTER* a
 * performed transfer, all results from this function are undefined until the
 * transfer is completed.
 */
CURL_EXTERN CURLcode curl_easy_getinfo(CURL *curl, CURLINFO info, ...);
*)
function curl_easy_getinfo(curl:THandle;info:TCurlInfo):TCurlCode;cdecl varargs;
(*/*
 * NAME curl_easy_duphandle()
 *
 * DESCRIPTION
 *
 * Creates a new curl session handle with the same options set for the handle
 * passed in. Duplicating a handle could only be a matter of cloning data and
 * options, internal state info and things like persistent connections cannot
 * be transferred. It is useful in multithreaded applications when you can run
 * curl_easy_duphandle() for each new thread to avoid a series of identical
 * curl_easy_setopt() invokes in every thread.
 */
CURL_EXTERN CURL* curl_easy_duphandle(CURL *curl);
*)
function curl_easy_duphandle(curl:THandle):THandle;cdecl;
(*/*
 * NAME curl_easy_reset()
 *
 * DESCRIPTION
 *
 * Re-initializes a CURL handle to the default values. This puts back the
 * handle to the same state as it was in when it was just created.
 *
 * It does keep: live connections, the Session ID cache, the DNS cache and the
 * cookies.
 */
CURL_EXTERN void curl_easy_reset(CURL *curl);
*)
procedure curl_easy_reset(curl:THandle);cdecl;
(*/*
 * NAME curl_easy_recv()
 *
 * DESCRIPTION
 *
 * Receives data from the connected socket. Use after successful
 * curl_easy_perform() with CURLOPT_CONNECT_ONLY option.
 */
CURL_EXTERN CURLcode curl_easy_recv(CURL *curl, void *buffer, size_t buflen,
                                    size_t *n);
*)
function curl_easy_recv(curl:THandle;buffer:Pointer;buflen:Integer;var n:Integer):TCurlCode;cdecl;
(*
/*
 * NAME curl_easy_send()
 *
 * DESCRIPTION
 *
 * Sends data over the connected socket. Use after successful
 * curl_easy_perform() with CURLOPT_CONNECT_ONLY option.
 */
CURL_EXTERN CURLcode curl_easy_send(CURL *curl, const void *buffer,
                                    size_t buflen, size_t *n);
*)
function curl_easy_send(curl:THandle;const buffer:Pointer;buflen:Integer;var n:Integer):TCurlCode;cdecl;
implementation
const libcurl='libcurl.dll';
function curl_formadd(httppost,lastpost:PPCurlHttpPost):TCUrlFormCode;cdecl varargs;external libcurl;
function curl_formget(var form:TCurlHttpPost;arg:Pointer;append:TCurrFormGetCallback):Integer;cdecl;external libcurl;
procedure curl_formfree(var form:TCurlHttpPost);cdecl;cdecl;external libcurl;
function curl_getenv(const variable:PAnsiChar):PAnsiChar;cdecl;external libcurl;
function curl_version:PAnsiChar;cdecl;external libcurl;
function curl_easy_escape(handle:THandle;const str:PAnsiChar;length:Integer):PAnsiChar;cdecl;external libcurl;
function curl_escape(const str:PAnsiChar;length:Integer):PAnsiChar;cdecl;external libcurl;
function curl_easy_unescape(handle:THandle;const str:PAnsiChar;length:Integer;var outlength:Integer):PAnsiChar;cdecl;external libcurl;
function curl_unescape(const str:PAnsiChar;length:Integer):PAnsiChar;cdecl;external libcurl;
procedure curl_free(p:Pointer);cdecl;external libcurl;
function curl_global_init(flags:Longint):TCurlCode;cdecl;external libcurl;
function curl_global_init_mem(flags:Longint;m:TCUrlMallocCallback;f:TCurlFreeCallback;r:TCurlReallocCallback;s:TCurlStrdupCallback;c:TCurlCallocCallback):TCurlCode;cdecl;external libcurl;
procedure curl_global_cleanup;cdecl;external libcurl;
function curl_slist_append(list:PCurlSList;const s:PAnsiChar):PCurlSList;cdecl;external libcurl;
procedure curl_slist_free_all(list:PCurlSList);cdecl;external libcurl;
function curl_getdate(const p:PAnsiChar;var unused:longint):longint;cdecl;external libcurl;
function curl_share_init:Pointer;cdecl;external libcurl;
function curl_share_setopt(sh:Pointer;option:TCurlSHOption):TCurlSHCode;cdecl varargs;external libcurl;
function curl_share_cleanup(sh:Pointer):TCURLSHCode;cdecl;external libcurl;
function curl_version_info(ver:TCurlVersion):TCurlVersionInfoData;cdecl;external libcurl;
function curl_easy_strerror(code:TCurlCode):PAnsiChar;cdecl;external libcurl;
function curl_share_strerror(ACode:TCurlShCode):PAnsiChar;cdecl;external libcurl;
function curl_easy_pause(handle:THandle;bitmask:Integer):TCurlCode;cdecl;external libcurl;
//multi.h
function curl_multi_init:THandle;cdecl;external libcurl;
function curl_multi_cleanup(multi_handle:THandle):TCurlMCode;cdecl;external libcurl;
function curl_multi_add_handle(multi_handle:THandle;curl_handle:THandle):TCurlMCode;cdecl;external libcurl;
function curl_multi_remove_handle(multi_handle:THandle;curl_handle:THandle):TCurlMCode;cdecl;external libcurl;
function curl_multi_fdset(multi_handle:THandle;var read_fd_set,write_fd_set,exc_fd_set:Tfdset;var max_fd:Integer):TCurlMCode;cdecl;external libcurl;
function curl_multi_wait(multi_handle:THandle;var extra_fds:TCurlWaitFd;extra_nfds:Cardinal;timeout_ms:Integer;var ret:Integer):TCurlMCode;cdecl;external libcurl;
function curl_multi_perform(multi_handle:THandle;var running_handles:Integer):TCurlMCode;cdecl;external libcurl;
function curl_multi_info_read(multi_handle:THandle;var msgs_in_queue:Integer):PCUrlMsg;cdecl;external libcurl;
function curl_multi_strerror(code:TCurlMCode):PAnsiChar;cdecl;external libcurl;
function curl_multi_socket(multi_handle:THandle;s:TSocket;var running_handles:Integer) :TCurlMCode;cdecl;external libcurl;
function curl_multi_socket_action(multi_handle:THandle;s:TSocket;ev_bitmask:Integer;var running_handles:Integer):TCurlMCode;cdecl;external libcurl;
function curl_multi_socket_all(multi_handle:THandle;var running_handles:Integer):TCurlMCode;cdecl;external libcurl;
function curl_multi_timeout(multi_handle:THandle;var milliseconds:LongInt):TCurlMCode;cdecl;external libcurl;
function curl_multi_setopt(multi_handle:THandle;option:TCUrlMOption):TCurlMCode;cdecl varargs;external libcurl;
function curl_multi_assign(multi_handle:THandle;sockfd:TSocket;sockp:Pointer):TCurlMCode;cdecl;external libcurl;
//multi.h 结束
//easy.h
function curl_easy_init:THandle;cdecl;external libcurl;
function curl_easy_setopt(curl:THandle;option:TCurlOption):TCurlCode;cdecl varargs;external libcurl;
function curl_easy_perform(curl:THandle):TCurlCode;cdecl;external libcurl;
procedure curl_easy_cleanup(curl:THandle);cdecl;external libcurl;
function curl_easy_getinfo(curl:THandle;info:TCurlInfo):TCurlCode;cdecl varargs;external libcurl;
function curl_easy_duphandle(curl:THandle):THandle;cdecl;external libcurl;
procedure curl_easy_reset(curl:THandle);cdecl;external libcurl;
function curl_easy_recv(curl:THandle;buffer:Pointer;buflen:Integer;var n:Integer):TCurlCode;cdecl;external libcurl;
function curl_easy_send(curl:THandle;const buffer:Pointer;buflen:Integer;var n:Integer):TCurlCode;cdecl;external libcurl;
end.
