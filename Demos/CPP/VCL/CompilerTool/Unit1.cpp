//
//                       _oo0oo_
//                      o8888888o
//                      88" . "88
//                      (| -_- |)
//                      0\  =  /0
//                    ___/`---'\___
//                  .' \\|     |// '.
//                 / \\|||  :  |||// \
//                / _||||| -:- |||||- \
//               |   | \\\  -  /// |   |
//               | \_|  ''\---/''  |_/ |
//               \  .-\__  '-'  ___/-. /
//             ___'. .'  /--.--\  `. .'___
//          ."" '<  `.___\_<|>_/___.' >' "".
//         | | :  `- \`.;`\ _ /`;.`/ - ` : | |
//         \  \ `_.   \_ __\ /__ _/   .-` /  /
//     =====`-.____`.___ \_____/___.-`___.-'=====
//                       `=---='
//
//
//     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//
//               佛祖保佑         永无BUG
//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "Unit2.h"
#include "Unit3.h"
#include "Registry.hpp"
#include "IniFiles.hpp"
#include "shlobj.h"
#include "ShLwApi.hpp"
#include "QLang.hpp"
//---------------------------------------------------------------------------
//#pragma package(smart_init)
#pragma resource "*.dfm"
#pragma link "Qlang"
#pragma comment(lib,"ShLwApi")
TForm_Main *Form_Main;
wchar_t MyszDir[MAX_PATH];
//---------------------------------------------------------------------------
__fastcall TForm_Main::TForm_Main(TComponent* Owner)
	: TForm(Owner)
{
 DotNETPath=L"";
 OnlyFromConfig=false;
 SelectLangID=0;
 RegPath=new TStringList();
 RegPath->Add(L"Software\\Borland\\Delphi");
 RegPath->Add(L"Software\\Borland\\C++Builder");
 RegPath->Add(L"Software\\Borland\\BDS");
 RegPath->Add(L"Software\\Codegear\\BDS");
 RegPath->Add(L"Software\\Embarcadero\\BDS");
 ExtList=new TStringList();
 ExtList->Add(L".pas");
 ExtList->Add(L".dpr");
 ExtList->Add(L".dpk");
 ExtList->Add(L".dproj");
 ExtList->Add(L".cpp");
 ExtList->Add(L".bpr");
 ExtList->Add(L".bpk");
 ExtList->Add(L".cbproj");
 CompilerList=new TStringList();
 FileList=new TStringList();
 CustomCfg=new TQJson();
 JobGroup=new TQJobGroup(true);
}
//---------------------------------------------------------------------------
int CALLBACK BrowseCallbackProc(HWND hwnd,UINT uMsg,LPARAM lParam,LPARAM lpData)
{
 if(uMsg == BFFM_INITIALIZED)
   {
	SendMessage(hwnd,BFFM_SETSELECTION,true,(LPARAM)MyszDir);
   }
 return 0;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall FolderBrowser(UnicodeString CurrPath)
{
 wchar_t szDir[MAX_PATH];
 UnicodeString Directory=L"";
 wcscpy(MyszDir,CurrPath.c_str());
 ITEMIDLIST *pidl;
 BROWSEINFO bi;
 ZeroMemory(&bi, sizeof(bi));
 bi.hwndOwner = Application->Handle;
 bi.pidlRoot = NULL;
 bi.pszDisplayName = szDir;
 bi.lpszTitle = _(L"请选择目录").c_str();
 bi.ulFlags = BIF_STATUSTEXT|BIF_USENEWUI|BIF_RETURNONLYFSDIRS;
 bi.lpfn = BrowseCallbackProc;
 bi.lParam = 0;
 bi.iImage = 0;
 pidl = SHBrowseForFolder(&bi);
 if(pidl != NULL)
   {
	if(SHGetPathFromIDList(pidl, szDir))
	  {
	   Directory = szDir;
	  }
   }
 return Directory;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall Repl(UnicodeString Str,UnicodeString Old,UnicodeString New)
{
 return StringReplace(Str,Old,New,TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetQuotePaths(UnicodeString Paths,bool Quote)
{
 UnicodeString Str;
 TStringList *List=new TStringList();
 List->StrictDelimiter=true;
 List->Delimiter=';';
 List->DelimitedText=Paths;
 Paths=L"";
 for(int i=0;i<List->Count;i++)
   {
	Str=List->Strings[i].Trim();
	Str=Repl(Str,L"\"",L"").Trim();
	if(Str!=L"")
	  {
	   if(Paths!=L"")
		 {
		  Paths+=L";";
		 }
	   if(Quote)
		 Paths+=L"\""+Str+L"\"";
	   else
		 Paths+=Str;
	  }
   }
 delete List;
 return Paths;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetCompleteFileName(UnicodeString CurrPath,UnicodeString FileName)
{
 wchar_t Dest[MAX_PATH]={0};
 PathCombine(Dest,CurrPath.c_str(),FileName.c_str());
 FileName=Dest;
 return FileName;
}
//---------------------------------------------------------------------------
int __fastcall FileOrDir(UnicodeString FileName)
{
 if(DirectoryExists(FileName))
   return 2;
 else if(FileExists(FileName))
   return 1;
 return 0;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall MyGetCurrentDirectory()
{
 wchar_t Dest[MAX_PATH]={0};
 GetCurrentDirectory(MAX_PATH,Dest);
 UnicodeString Str=Dest;
 return Str;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall GetNeedFile(UnicodeString FileName)
{
 TStringList *List=new TStringList();
 UnicodeString Str;
 List->LoadFromFile(FileName);
 List->Text=StringReplaceWithW(List->Text,L"{",L"}",L"{},",false,true,-1);
 List->Text=StringReplaceWithW(List->Text,L"/*",L"*/",L"",false,false,-1);
 int p;
 bool r=false;
 for(int i=0;i<List->Count;)
   {
    Str=List->Strings[i].LowerCase().Trim();
    p=Str.Pos(L"//");
    if(p>0)
      {
       Str=Str.SubString(1,p-1);
       List->Strings[i]=Str;
      }
    if(Str==L"")
      {
       List->Delete(i);
      }
    else if(!r&&Str!=L"requires"&&Str!=L"requires "&&Str!=L"uses"&Str.SubString(1,5)!=L"uses ")
      {
       List->Delete(i);
      }
    else if(!r&&(Str==L"requires"||Str==L"requires "||Str==L"uses"||Str.SubString(1,5)==L"uses "))
      {
       r=true;
       if(Str==L"requires"||Str==L"uses")
         List->Delete(i);
       else if(Str.SubString(1,5)==L"uses ")
         {
          Str=Str.SubString(6,Str.Length()-5);
          List->Strings[i]=Str;
          i++;
         }
       else if(Str.SubString(1,9)==L"requires ")
         {
          Str=Str.SubString(9,Str.Length()-9);
          List->Strings[i]=Str;
          i++;
         }
       if(Str.Pos(L";")>0)
         {
          r=false;
         }
      }
    else if(r&&Str.Pos(L";")>0)
      {
       r=false;
       i++;
      }
    else if(r)
      {
       i++;
      }
   }
 Str=List->Text;
 delete List;
 Str=StringReplaceW(Str,L" ",L"",TReplaceFlags()<<rfReplaceAll);
 Str=StringReplaceW(Str,L"\r\n",L"",TReplaceFlags()<<rfReplaceAll);
 Str=StringReplaceW(Str,L"\r",L"",TReplaceFlags()<<rfReplaceAll);
 Str=StringReplaceW(Str,L"\n",L"",TReplaceFlags()<<rfReplaceAll);
 Str=StringReplaceW(Str,L";",L"|",TReplaceFlags()<<rfReplaceAll);
 Str=StringReplaceW(Str,L",",L"|",TReplaceFlags()<<rfReplaceAll);
 if(Str.Trim()!=L"")
   Str=L"|"+Str.Trim();
 return Str;
}
//---------------------------------------------------------------------------
int __fastcall Contains(UnicodeString FileName,UnicodeString Unit)
{
 UnicodeString ContainsStr=GetNeedFile(FileName);
 Unit=Unit.LowerCase();
 if(ContainsStr.Pos(L"|"+Unit+L"|")<1)
   return 0;
 else if((ContainsStr.Pos(L"|"+Unit+L"|")>0||ContainsStr.Pos(L"|"+Unit+L".")>0)&&ContainsStr.Pos(L"|{}|")<1)
   return 1;
 else
   return 2;
}
//---------------------------------------------------------------------------
int __fastcall IsFMX(UnicodeString FileName)
{
 UnicodeString Tmp,ExtName=ExtractFileExt(FileName).LowerCase();
 if(ExtName==L".dproj"||ExtName==L".cbproj")
   {
    TStringList *List=new TStringList();
    List->LoadFromFile(FileName);
    int r=0;
    if(List->Text.LowerCase().Pos(L"<frameworktype>fmx</frameworktype>")>0)
      r=1;
    delete List;
    return r;
   }
 else
   return Contains(FileName,L"FMX");
}
// ---------------------------------------------------------------------------
int __fastcall IsNoVCL(UnicodeString FileName)
{
 return Contains(FileName,L"VCL");
}
//---------------------------------------------------------------------------
bool __fastcall IsDesignOnly(UnicodeString FileName)
{
 if(!FileExists(FileName))
   return true;
 bool r=false;
 UnicodeString ExtName=ExtractFileExt(FileName).LowerCase();
 TStringList *List=new TStringList();
 List->LoadFromFile(FileName);
 if(ExtName==L".dpk")
   {
	for(int i=0;i<List->Count;i++)
	  {
	   ExtName=List->Strings[i].LowerCase();
	   if(ExtName==L"{$designonly}")
		 {
		  r=true;
		  break;
         }
	   else if(ExtName==L"requires")
		 {
		  for(;i<List->Count;i++)
			{
			 ExtName=List->Strings[i].LowerCase();
			 if(ExtName.Pos(L"designide")>0)
			   {
				r=true;
				break;
			   }
			 if(ExtName.Pos(L";")>0)
			   break;
			}
		  break;
		 }
	  }
   }
 else if(ExtName==L".dproj"||ExtName==L".cbproj")
   {
	if(List->Text.LowerCase().Pos(L"<designonlypackage>true</designonlypackage>")>0)
	  r=true;
   }
 delete List;
 return r;
}
//---------------------------------------------------------------------------
bool __fastcall IsAdministrator()
{
 DWORD RelativeGroupID = DOMAIN_ALIAS_RID_ADMINS;
 void * psidAdmin;
 HANDLE Token;
 DWORD Count;
 PTokenGroups TokenInfo;
 bool HaveToken;
 OSVERSIONINFO ov;
 SID_IDENTIFIER_AUTHORITY NtAuthority = SECURITY_NT_AUTHORITY;
 ov.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
 GetVersionEx(&ov);
 if(ov.dwPlatformId != VER_PLATFORM_WIN32_NT)
   {
	return true;
   }
 psidAdmin = NULL;
 TokenInfo = NULL;
 HaveToken = false;
 try
   {
	Token = 0;
	HaveToken = OpenThreadToken(GetCurrentThread(), TOKEN_QUERY, True,
		&Token);
	if((!HaveToken) && (GetLastError() == ERROR_NO_TOKEN))
	  {
		HaveToken = OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY,
				&Token);
	  }
	if(HaveToken)
	  {
	   if(!AllocateAndInitializeSid(&NtAuthority,2,SECURITY_BUILTIN_DOMAIN_RID, RelativeGroupID,0,0,0,0,0,0,&psidAdmin))
		 {
		  return false;
		 }
	  }
	else
	  {
	   ShowMessage(SysErrorMessage(GetLastError()));
	   return false;
	  }
	if(GetTokenInformation(Token,TokenGroups,NULL,0,&Count)||(GetLastError()!=ERROR_INSUFFICIENT_BUFFER))
	  {
	   return false;
	  }
	TokenInfo = (PTokenGroups)AllocMem(Count);
	if(!GetTokenInformation(Token,TokenGroups,TokenInfo,Count,&Count))
	  {
	   return false;
	  }
	for(DWORD I=0;I<TokenInfo->GroupCount;I++)
	  {
	   if(EqualSid(psidAdmin,TokenInfo->Groups[I].Sid))
		 {
		  // consider denied ACE with Administrator SID
		  return (TokenInfo->Groups[I].Attributes&SE_GROUP_USE_FOR_DENY_ONLY)!=SE_GROUP_USE_FOR_DENY_ONLY;
		 }
	  }
   }
 __finally
   {
	if(TokenInfo)
	  {
	   FreeMemory(TokenInfo);
	  }
	if(HaveToken)
	  {
	   CloseHandle(Token);
	  }
	if(psidAdmin)
	  {
	   FreeSid(psidAdmin);
	  }
   }
 return false;
}
//---------------------------------------------------------------------------
AnsiString __fastcall MyGetFileVersion(UnicodeString FileName)
{
 int iVerInfoSize;
 TCHAR *pBuf;
 AnsiString asVer=L"";
 VS_FIXEDFILEINFO *pVsInfo;
 unsigned int iFileInfoSize = sizeof( VS_FIXEDFILEINFO );
 iVerInfoSize = GetFileVersionInfoSize(FileName.c_str(), NULL);
 if(iVerInfoSize!= 0)
   {
	pBuf = new TCHAR[iVerInfoSize];
	if(GetFileVersionInfo(FileName.c_str(),0, iVerInfoSize, pBuf ) )
	  {
	   if(VerQueryValue(pBuf,L"\\",(void **)&pVsInfo,&iFileInfoSize))
		 {
		  asVer = IntToStr( HIWORD(pVsInfo->dwFileVersionMS) )+".";
		  asVer += IntToStr( LOWORD(pVsInfo->dwFileVersionMS) )+".";
		  asVer += IntToStr( HIWORD(pVsInfo->dwFileVersionLS) )+".";
		  asVer += IntToStr( LOWORD(pVsInfo->dwFileVersionLS) );
		 }
	   }
	delete []pBuf;
	pBuf=NULL;
   }
 return asVer;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::FormDestroy(TObject *Sender)
{
 LangManager->RemoveListener(DoLanguageChanged);
 FreeObject(JobGroup);
 Workers->Clear();
 delete RegPath;
 delete ExtList;
 delete CompilerList;
 delete FileList;
 delete CustomCfg;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::FormCreate(TObject *Sender)
{
 CurrentDir=MyGetCurrentDirectory();
 DragAcceptFiles(this->Handle,true);
 ComSpec=GetEnvironmentVariable(L"ComSpec");
 GetDotNETPath();
 GetOnlyFromConfig();


 if(SelectLangID==0)
   SelectLangID=GetSystemDefaultLangID();
 if(LangManager->LanguageExists(SelectLangID))
   {
	LangManager->ActiveLocale=SelectLangID;
   }
 LangManager->AddListener(DoLanguageChanged);
 AppendMenu(GetSystemMenu(Handle,false),MF_SEPARATOR,0,L"");
 AppendMenu(GetSystemMenu(Handle,false),MF_STRING,200,_(L"关于(&A)").c_str());
 AppendMenu(GetSystemMenu(Handle,false),MF_SEPARATOR,0,L"");
 AppendMenu(GetSystemMenu(Handle,false),MF_STRING|MF_UNCHECKED,201,_(L"只从配置文件读取").c_str());
 AppendMenu(GetSystemMenu(Handle,false),MF_SEPARATOR,0,L"");

 HMENU LangPopMenu=CreatePopupMenu();
 ChangeLangID->Items->Add(L"简体中文(默认)");
 AppendMenu(LangPopMenu,MF_STRING,250,L"简体中文(默认)");
 for(int i=1;i<LangManager->Count;i++)
   {
	AppendMenu(LangPopMenu,MF_STRING,250+i,LangManager->Languages[i]->Name.c_str());
	ChangeLangID->Items->Add(LangManager->Languages[i]->Name);
   }
 ChangeLangID->ItemIndex=LangManager->ActiveIndex;
 AppendMenu(GetSystemMenu(Handle,false),MF_STRING|MF_POPUP,(UINT)LangPopMenu,_(L"多语言(&L)").c_str());

 if(!OnlyFromConfig)
   GetCompilerFromReg();
 else
   CheckMenuItem(GetSystemMenu(Handle,false),201,MF_CHECKED);
 GetCompilerFromConfig();

 for(int i=0;i<CompilerList->Count;i++)
   {
	if(CompilerList->ValueFromIndex[i].Pos(L"32")>0)
	  {
	   TNxCheckBoxColumn *NCBC32=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBC32->Name=CompilerList->Names[i]+L"_32";
	   NCBC32->Header->Caption=CompilerList->Names[i]+_(L" 32位");
	   NCBC32->Header->Alignment=taCenter;
	   NCBC32->Alignment=taCenter;
	   NCBC32->Width=88;
	   NCBC32->Options>>coCanSort;
	  }
	if(CompilerList->ValueFromIndex[i].Pos(L"64")>0)
	  {
	   TNxCheckBoxColumn *NCBC64=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBC64->Name=CompilerList->Names[i]+L"_64";
	   NCBC64->Header->Caption=CompilerList->Names[i]+_(L" 64位");
	   NCBC64->Header->Alignment=taCenter;
	   NCBC64->Alignment=taCenter;
	   NCBC64->Width=88;
	   NCBC64->Options>>coCanSort;
	  }
	if(CompilerList->ValueFromIndex[i].Pos(L"AD")>0)
	  {
	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBCAD->Name=CompilerList->Names[i]+L"_AD";
	   NCBCAD->Header->Caption=CompilerList->Names[i]+_(L" 安卓");
	   NCBCAD->Header->Alignment=taCenter;
	   NCBCAD->Alignment=taCenter;
	   NCBCAD->Width=88;
	   NCBCAD->Options>>coCanSort;
	  }
	if(CompilerList->ValueFromIndex[i].Pos(L"A6")>0)
	  {
	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBCAD->Name=CompilerList->Names[i]+L"_A6";
	   NCBCAD->Header->Caption=CompilerList->Names[i]+_(L" 安卓64");
	   NCBCAD->Header->Alignment=taCenter;
	   NCBCAD->Alignment=taCenter;
	   NCBCAD->Width=88;
	   NCBCAD->Options>>coCanSort;
	  }
	if(CompilerList->ValueFromIndex[i].Pos(L"IO")>0)
	  {
	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBCAD->Name=CompilerList->Names[i]+L"_IO";
	   NCBCAD->Header->Caption=CompilerList->Names[i]+_(L" IOS");
	   NCBCAD->Header->Alignment=taCenter;
	   NCBCAD->Alignment=taCenter;
	   NCBCAD->Width=88;
	   NCBCAD->Options>>coCanSort;
	  }
	if(CompilerList->ValueFromIndex[i].Pos(L"I6")>0)
	  {
	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBCAD->Name=CompilerList->Names[i]+L"_I6";
	   NCBCAD->Header->Caption=CompilerList->Names[i]+_(L" IOS64");
	   NCBCAD->Header->Alignment=taCenter;
	   NCBCAD->Alignment=taCenter;
	   NCBCAD->Width=88;
	   NCBCAD->Options>>coCanSort;
	  }
	if(CompilerList->ValueFromIndex[i].Pos(L"OS")>0)
	  {
	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBCAD->Name=CompilerList->Names[i]+L"_OS";
	   NCBCAD->Header->Caption=CompilerList->Names[i]+_(L" OSX");
	   NCBCAD->Header->Alignment=taCenter;
	   NCBCAD->Alignment=taCenter;
	   NCBCAD->Width=88;
	   NCBCAD->Options>>coCanSort;
	  }
	if(CompilerList->ValueFromIndex[i].Pos(L"O6")>0)
	  {
	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBCAD->Name=CompilerList->Names[i]+L"_O6";
	   NCBCAD->Header->Caption=CompilerList->Names[i]+_(L" OSX64");
	   NCBCAD->Header->Alignment=taCenter;
	   NCBCAD->Alignment=taCenter;
	   NCBCAD->Width=88;
	   NCBCAD->Options>>coCanSort;
	  }
   }
 if(ParamCount()==1&&FileExists(ParamStr(1)))
   {
	  if(ExtractFileExt(ParamStr(1)).LowerCase()==L".compilercfg")
	    {
	     CfgName=ParamStr(1);
	     LoadProject(CfgName);
	    }
   }
 else if(ParamCount()>0)
   {
    if(FindCmdLineSwitch(L"NU",TSysCharSet()<<'\\'<<'/'<<'-',true))
      {
       if(RunWithCmdline())
         {
          Application->ShowMainForm=false;
          Application->Terminate();
         }
      }
    else
      {
       Workers->Post(DelayRunWithCmdline,NULL,true);
      }
   }
 if(IsAdministrator())
   {
	  PC->ActivePage=TabReadme;
	  P3->Visible=true;
	  if(!FileTypeIsRegister(L"CompilerCfg"))
	    {
		 RegisterFileType(L"CompilerCfg",Application->ExeName);
	     PC->ActivePage=TabReadme;
	    }
	  else
	    PC->ActivePage=TabProject;
   }
 else
   {
	  PC->ActivePage=TabProject;
	  P3->Visible=false;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GetDotNETPath()
{
 TRegistry *Registry = new TRegistry();
 Registry->Access=KEY_READ;
 Registry->RootKey=HKEY_LOCAL_MACHINE;
 UnicodeString DotNetRootPath;
 if(Registry->OpenKey(L"SOFTWARE\\Microsoft\\.NETFramework",false))
   {
	if(Registry->ValueExists(L"InstallRoot"))
	  {
	   DotNETPath=ExcludeTrailingPathDelimiter(Registry->ReadString(L"InstallRoot"));
      }
   }
 if(DotNETPath==L"")
   return;
 UnicodeString DotNETVer=L"";
 TSearchRec sr;
 if(FindFirst(DotNETPath+L"\\"+L"*.*", faAnyFile, sr) == 0)
   {
	do
	  {
	   if((sr.Attr & faDirectory) && sr.Name!=L"." && sr.Name!=L"..")
		 {
		  if(FileExists(DotNETPath+L"\\"+sr.Name+L"\\MsBuild.exe"))
			DotNETVer=sr.Name;
		 }
	  } while (FindNext(sr) == 0);
	FindClose(sr);
   }
 if(DotNETVer==L"")
   {
	DotNETPath=L"";
	return;
   }
 DotNETPath+=L"\\"+DotNETVer;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TForm_Main::PipeCall(UnicodeString CmdLine,UnicodeString Path)
{
 SECURITY_ATTRIBUTES sa;
 HANDLE hRead,hWrite;
 sa.nLength = sizeof(SECURITY_ATTRIBUTES);
 sa.lpSecurityDescriptor = NULL; //使用系统默认的安全描述符
 sa.bInheritHandle = TRUE; //创建的进程继承句柄
 if (!CreatePipe(&hRead,&hWrite,&sa,0)) //创建匿名管道
 {
  return _(L"匿名管道创建失败，错误号：")+IntToStr((int)GetLastError())+_(L"！");
 }
 STARTUPINFO si;
 PROCESS_INFORMATION pi;
 memset(&si,0,sizeof(STARTUPINFO));
 si.cb = sizeof(STARTUPINFO);
 GetStartupInfo(&si);
 si.hStdError = hWrite;
 si.hStdOutput = hWrite; //新创建进程的标准输出连在写管道一端
 si.wShowWindow = SW_HIDE; //隐藏窗口
 si.dwFlags = STARTF_USESHOWWINDOW | STARTF_USESTDHANDLES;
 if(Path!=L"")
   SetCurrentDirectory(Path.c_str());
 if (!CreateProcess(NULL,CmdLine.c_str(),NULL,NULL,TRUE,NULL,NULL,Path.c_str(),&si,&pi)) //创建子进程
 {
  return _(L"进程创建失败，错误号：")+IntToStr((int)GetLastError())+_(L"！");
 }
 CloseHandle(hWrite); //关闭管道句柄
 char buffer[4096] = {0};
 DWORD bytesRead;
 DWORD TotalBytesAvail;
 CmdLine=L"";
 while(PeekNamedPipe(hRead,NULL,0,NULL,&TotalBytesAvail,NULL))
   {
	memset(buffer,0,sizeof(buffer));
	if(!ReadFile(hRead,buffer,Min(sizeof(buffer),(int)TotalBytesAvail),&bytesRead,NULL))
	  break;
	CmdLine+= buffer;
   }
 CloseHandle(hRead);
 return CmdLine;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GetCompilerFromReg()
{
 TRegistry *Registry = new TRegistry();
 Registry->Access=KEY_READ;
 Registry->RootKey=HKEY_LOCAL_MACHINE;
 TStringList *List=new TStringList();
 TStringList *List2=new TStringList();
 UnicodeString RegPathName,KeyName,Key,Type,RootDir,BrowsingPath;
 int IDEVersion;
 for(int i=0;i<RegPath->Count;i++)
   {
	if(i==0)
	  Type=L"DELPHI";
	else if(i==1)
	  Type=L"BCB";
	else
	  Type=L"BDS";
	RegPathName=RegPath->Strings[i];
	if(Registry->KeyExists(RegPathName))
	  {
	   Registry->OpenKey(RegPathName,false);
	   Registry->GetKeyNames(List);
	   Registry->CloseKey();
	   for(int j=0;j<List->Count;j++)
		 {
		  Name=L"";
		  RootDir=L"";
		  Key=List->Strings[j];
		  if(IntToStr((int)StrToFloatDef(Key,0))+L".0"==Key)
			{
			 IDEVersion=(int)StrToFloatDef(Key,0);
			 KeyName=RegPathName+L"\\"+Key;
			 Registry->OpenKey(KeyName,false);
			 List2->Clear();
			 Registry->GetValueNames(List2);
			 if(List2->IndexOf(L"RootDir")>=0)
			   {
				RootDir=ExcludeTrailingPathDelimiter(Registry->ReadString(L"RootDir"));
				if(RootDir==L"")
				  continue;
				else if(!DirectoryExists(RootDir))
				  continue;
				else
				  {
				   TQJson *QJson=new TQJson();
				   QJson->ForcePath("KeyName")->AsString=KeyName;
				   QJson->ForcePath("App")->AsString=Registry->ReadString(L"App");
				   QJson->ForcePath("RootDir")->AsString=RootDir;
				   QJson->ForcePath("Type")->AsString=Type;
				   QJson->ForcePath("IDEVersion")->AsString=IDEVersion;
				   CheckCompiler(QJson);
				   if(QJson->ForcePath("RootDir")->AsString==RootDir)
					 {
					  Key=L"Library";
					  Registry->OpenKey(Key,false);
					  if(Registry->HasSubKeys())
						{
						 Registry->OpenKey(L"Win32",false);
						 BrowsingPath=Registry->ReadString("Browsing Path");
						}
					  else
						{
						 BrowsingPath=Registry->ReadString(L"Browsing Path");
						}
					  QJson->ForcePath("BrowsingPath")->AsString=BrowsingPath;
					  if(CompilerList->IndexOfName(QJson->ForcePath("Ver")->AsString)<0)
					   {
						CompilerList->AddObject(QJson->ForcePath("Ver")->AsString+L"="+QJson->ForcePath("Platform")->AsString,QJson);
						CreateTabSheet(QJson);
					   }
					 }
				   else
					 {
					  delete QJson;
					 }
				  }
			   }
			}
		  Registry->CloseKey();
		 }
	   Registry->CloseKey();
	  }
	Registry->CloseKey();
   }
 delete Registry;
 delete List;
 delete List2;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::CheckCompiler(TQJson *QJson)
{
 UnicodeString Type,RootDir,KeyName,Ver,Name,Platform,Cmd;
 int Tmp;
 float Version;
 Type=QJson->ForcePath("Type")->AsString;
 RootDir=QJson->ForcePath("RootDir")->AsString;
 if(!DirectoryExists(RootDir))
   {
	QJson->ForcePath("RootDir")->AsString=L"";
	return;
   }
 Platform=L"|";
 if(FileExists(RootDir+L"\\Bin\\Dcc32.exe"))
   Platform+=L"D32|";
 if(FileExists(RootDir+L"\\Bin\\Bcc32.exe"))
   Platform+=L"C32|";
 if(FileExists(RootDir+L"\\Bin\\Dcc64.exe"))
   Platform+=L"D64|";
 if(FileExists(RootDir+L"\\Bin\\Bcc64.exe"))
   Platform+=L"C64|";
 if(FileExists(RootDir+L"\\Bin\\Bcc64.exe"))
   Platform+=L"C64|";
 if(FileExists(RootDir+L"\\Bin\\Dccaarm.exe"))
   Platform+=L"DAD|";
 if(FileExists(RootDir+L"\\Bin\\Bccaarm.exe"))
   Platform+=L"CAD|";
 if(FileExists(RootDir+L"\\Bin\\Dccaarm64.exe"))
   Platform+=L"DA6|";
 if(FileExists(RootDir+L"\\Bin\\Bccaarm64.exe"))
   Platform+=L"CA6|";
 if(FileExists(RootDir+L"\\Bin\\Dcciosarm.exe"))
   Platform+=L"DIO|";
 if(FileExists(RootDir+L"\\Bin\\Bcciosarm.exe"))
   Platform+=L"CIO|";
 if(FileExists(RootDir+L"\\Bin\\Dcciosarm64.exe"))
   Platform+=L"DI6|";
 if(FileExists(RootDir+L"\\Bin\\Bcciosarm64.exe"))
   Platform+=L"CI6|";
 if(FileExists(RootDir+L"\\Bin\\Dccosx.exe"))
   Platform+=L"DOS|";
 if(FileExists(RootDir+L"\\Bin\\Bccosx.exe"))
   Platform+=L"COS|";
 if(FileExists(RootDir+L"\\Bin\\Dccosx64.exe"))
   Platform+=L"DO6|";
 if(FileExists(RootDir+L"\\Bin\\Bccosx64.exe"))
   Platform+=L"CO6|";
 if(Platform==L"|")
   {
	QJson->ForcePath("RootDir")->AsString=L"";
	return;
   }
 QJson->ForcePath("Platform")->AsString=Platform;
 GetCompilerInfo(QJson);
 GetEnvVar(QJson);
 if(QJson->ForcePath("RootDir")->AsString==L"")
   return;

}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::AcceptFiles(TMessage& Msg)
{
 wchar_t FileName[255];
 UnicodeString FileNameStr;
 int Count;
 Count=DragQueryFile((HDROP)Msg.WParam,0xffffffff,FileName,255);
 for(int i=0;i<Count;i++)
   {
	DragQueryFile((HDROP)Msg.WParam,i,FileName,255);
	FileNameStr=FileName;
	AddFile(FileNameStr);
   }
 DragFinish((HDROP)Msg.WParam);
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GetCompilerInfo(TQJson *QJson)
{
 UnicodeString App,RootDir,Type,Ver,Name,CMD=L"",Platform=L"";
 Platform=QJson->ForcePath("Platform")->AsString;
 RootDir=QJson->ForcePath("RootDir")->AsString;
 App=QJson->ForcePath("App")->AsString;
 int IDEVersion,MyRTLVersion,MyProductVersion;
 if(FileExists(App))
   {
	Type=QJson->ForcePath("Type")->AsString;
	CMD=App;
   }
 else if(FileExists(RootDir+L"\\Bin\\BDS.exe"))
   {
	Type="BDS";
	CMD=RootDir+L"\\Bin\\BDS.exe";
	QJson->ForcePath("App")->AsString=CMD;
   }
 else if(FileExists(RootDir+L"\\Bin\\Delphi32.exe"))
   {
	Type=L"DELPHI";
	CMD=RootDir+L"\\Bin\\Delphi32.exe";
	QJson->ForcePath("App")->AsString=CMD;
   }
 else if(FileExists(RootDir+L"\\Bin\\BCB.exe"))
   {
	Type=L"BCB";
	CMD=RootDir+L"\\Bin\\BCB.exe";
	QJson->ForcePath("App")->AsString=CMD;
   }
 if(CMD!=L"")
   {
	IDEVersion=GetFileVersion(CMD)/0x10000;
   }
 else if(Platform.Pos(L"D")>0)
   {
	IDEVersion=GetDelphiIDEVersion(QJson);
	if(IDEVersion<9)
	  Type=L"DELPHI";
	else
	  Type=L"BDS";
   }
 else
   {
    Type=L"BCB";
   }
 if(IDEVersion<6)
   {
	QJson->ForcePath("RootDir")->AsString=L"";
	return;
   }
 else if(Type==L"BCB")
   {
	Ver=L"C"+IntToStr(IDEVersion);
	Name=L"Borland C++ Builder "+IntToStr(IDEVersion);
   }
 else if(Type==L"DELPHI")
   {
	Ver=L"D"+IntToStr(IDEVersion);
	Name=L"Delphi "+IntToStr(IDEVersion);
   }
 else if(Type==L"BDS")
   {
	switch(IDEVersion)
	  {
	   case 9:Name=L"Delphi 2005";Ver=L"D2005";break;
	   case 10:Name=L"Borland Developer Studio 2006";Ver=L"RS2006";break;
	   case 11:Name=L"CodeGear Delphi 2007 for Win32";Ver=L"RS2007";break;
	   case 12:Name=L"CodeGear RAD Studio 2009";Ver=L"RS2009";break;
//	   case 12:Name=L"CodeGear RAD Studio 2008";Ver=L"RS2008";break;
	   case 13:Name=L"CodeGear RAD Studio 2009";Ver=L"RS2009";break;
	   case 14:Name=L"Embarcadero RAD Studio 2010";Ver=L"RS2010";break;
	   case 15:Name=L"Embarcadero RAD Studio XE";Ver=L"RSXE";break;
	   case 23:Name=L"Embarcadero RAD Studio 10 Seattle";Ver=L"RS10";break;
	   default:
			   if(IDEVersion>15&&IDEVersion<=22)
				 {
				  Name=L"Embarcadero RAD Studio XE"+IntToStr(IDEVersion-14);
				  Ver=L"RSXE"+IntToStr(IDEVersion-14);
				 }
			   else
				 {
				  Name=L"Embarcadero RAD Studio "+IntToStr(IDEVersion-13);
				  Ver=L"RS"+IntToStr(IDEVersion-13);
				 }
	  }
	if(Platform.Pos(L"C")>0&&Platform.Pos(L"D")>0)
	  Name+=L" (Delphi and C++ Builder)";
	else if(Platform.Pos(L"C")>0&&Platform.Pos(L"D")==0)
	  Name+=L" (Only C++ Builder)";
	else if(Platform.Pos(L"C")==0&&Platform.Pos(L"D")>0)
	  Name+=L" (Only Delphi)";
   }
 if(IDEVersion==11)
   {
	MyRTLVersion=185;
   }
 else if(IDEVersion<13)
   {
	MyRTLVersion=(IDEVersion+8)*10;
   }
 else
   {
	MyRTLVersion=(IDEVersion+7)*10;
   }
 if(IDEVersion>=20||IDEVersion<14)
   {
	MyProductVersion=IDEVersion-6;
   }
 else
   {
	MyProductVersion=IDEVersion-7;
   }
 QJson->ForcePath("Type")->AsString=Type;
 QJson->ForcePath("Name")->AsString=Name;
 QJson->ForcePath("Ver")->AsString=Ver;
 QJson->ForcePath("IDEVersion")->AsInteger=IDEVersion;
 QJson->ForcePath("RTLVersion")->AsInteger=MyRTLVersion;
 QJson->ForcePath("ProductVersion")->AsInteger=MyProductVersion;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::CreateTabSheet(TQJson *QJson)
{
 if(QJson->ForcePath("RootDir")->AsString==L"")
   return;
 if(QJson->ForcePath("IDEVersion")->AsInteger<6)
   return;
 TTabSheet *Tab=new TTabSheet(CompilerPC);
 Tab->PageControl=CompilerPC;
 CompilerPC->ActivePage=Tab;
 Tab->Name=L"Tab"+QJson->ForcePath("IDEVersion")->AsString;
 Tab->Caption=QJson->ForcePath("Ver")->AsString;
 TFrame_Compiler *Frame=new TFrame_Compiler(this);
 Frame->Name=L"Frame"+QJson->ForcePath("IDEVersion")->AsString;
 Frame->QJson=QJson;
 Frame->Parent=Tab;
 Frame->Align=alClient;
 Frame->Init();
 LangManager->Translate(Frame);
}
//---------------------------------------------------------------------------
int __fastcall TForm_Main::GetDelphiIDEVersion(TQJson *QJson)
{
 UnicodeString Platform,RootDir,CMD,Tmp=L"compiler version ",Str;
 float Version=0.0;
 int IDEVersion=0,p;
 Platform=QJson->ForcePath("Platform")->AsString;
 RootDir=QJson->ForcePath("RootDir")->AsString;
 if(Platform.Pos(L"D32")>0)
   CMD=L"\""+RootDir+L"\\Bin\\Dcc32.exe\"";
 else if(Platform.Pos(L"D64")>0)
   CMD=L"\""+RootDir+L"\\Bin\\Dcc64.exe\"";
 else
   return IDEVersion;
 Str=PipeCall(CMD);
 p=Str.Pos(Tmp);
 if(p>0)
   {
	Str=Str.SubString(p+17,4);
	Version=StrToFloatDef(Str,0.0);
   }
 if(Version>0)
   {
	if(Version>=19)
	  IDEVersion=Version-7;
	else if(Version==18.5)
	  IDEVersion=Version-7.5;
	else if(Version>8)
	  IDEVersion=Version-8;
	else
	  IDEVersion=0;
   }
 return IDEVersion;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnAddClick(TObject *Sender)
{
 OD->DefaultExt=L"";
 OD->FileName=L"";
 OD->Filter=_(L"所有可用文件")+L"|*.cpp;*.pas;*.bpr;*.dpr;*.bpk;*.dpk;*.cbproj;*.dproj|Delphi Unit|*.Pas|Delphi Project|*.dpr;*.dpk;*.dproj|C++ Builder Unit|*.cpp|C++ Builder Project|*.bpr;*.bpk;*.cbproj";
 OD->Title=_(L"请选择要打开的文件");
 if(OD->Execute())
   {
	for(int i=0;i<OD->Files->Count;i++)
	  {
	   AddFile(OD->Files->Strings[i]);
	  }
   }
}
//---------------------------------------------------------------------------
int  __fastcall TForm_Main::AddFile(UnicodeString FileName)
{
 UnicodeString CompleteFileName;
 if(CfgName!=L"")
   {
	CompleteFileName=GetCompleteFileName(ExtractFilePath(CfgName),FileName);
   }
 else
   CompleteFileName=FileName;
 if(FileList->IndexOf(CompleteFileName)>=0)
   return -1;
 UnicodeString Name,Platform,Tmp,ExtName=ExtractFileExt(CompleteFileName);
 int p=ExtList->IndexOf(ExtName);
 if(p<0)
   return -1;
 int i=NG->AddRow(1);
 if(FileExists(CompleteFileName))
   NG->CellByName["NGUse"][i]->AsBoolean=true;
 NG->CellByName["NGFile"][i]->AsString=FileName;
 for(int j=0;j<CompilerList->Count;j++)
   {
	Name=CompilerList->Names[j];
	Platform=CompilerList->ValueFromIndex[j];
	if(p<3)
	  {
	   if(Platform.Pos(L"D32")>0)
		 NG->CellByName[Name+L"_32"][i]->AsBoolean=true;
	   if(Platform.Pos(L"D64")>0)
		 NG->CellByName[Name+L"_64"][i]->AsBoolean=true;
	   if(Platform.Pos(L"DAD")>0)
		 NG->CellByName[Name+L"_AD"][i]->AsBoolean=true;
	   if(Platform.Pos(L"DA6")>0)
		 NG->CellByName[Name+L"_A6"][i]->AsBoolean=true;
	   if(Platform.Pos(L"DIO")>0)
		 NG->CellByName[Name+L"_IO"][i]->AsBoolean=true;
	   if(Platform.Pos(L"DI6")>0)
		 NG->CellByName[Name+L"_I6"][i]->AsBoolean=true;
	   if(Platform.Pos(L"DOS")>0)
		 NG->CellByName[Name+L"_OS"][i]->AsBoolean=true;
	   if(Platform.Pos(L"DO6")>0)
		 NG->CellByName[Name+L"_O6"][i]->AsBoolean=true;
	   if(Name.SubString(1,1)==L"C"&&p>0)
		 NG->CellByName[Name+L"_32"][i]->AsBoolean=false;

	   if(p>0&&Platform.Pos(L"D64")>0)
		 NG->CellByName[Name+L"_64"][i]->AsBoolean=!IsDesignOnly(CompleteFileName);
	   if(p>0&&Platform.Pos(L"DAD")>0)
		 NG->CellByName[Name+L"_AD"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(p>0&&Platform.Pos(L"DA6")>0)
		 NG->CellByName[Name+L"_A6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(p>0&&Platform.Pos(L"DIO")>0)
		 NG->CellByName[Name+L"_IO"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(p>0&&Platform.Pos(L"DI6")>0)
		 NG->CellByName[Name+L"_I6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(p>0&&Platform.Pos(L"DOS")>0)
		 NG->CellByName[Name+L"_OS"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(p>0&&Platform.Pos(L"DO6")>0)
		 NG->CellByName[Name+L"_O6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));

	   if(p>0&&IsFMX(CompleteFileName)==1&&Name.Pos(L"XE")==0)
		 {
		  if(Platform.Pos(L"D32")>0)
			NG->CellByName[Name+L"_32"][i]->AsBoolean=false;
		 }
	  }
	else if(p==3)
	  {
	   if(Platform.Pos(L"D32")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_32"][i]->AsBoolean=true;
	   if(NG->CellByName[Name+L"_32"][i]->AsBoolean&&Name.SubString(3,2)!=L"XE")
		 NG->CellByName[Name+L"_32"][i]->AsBoolean=!(IsFMX(CompleteFileName)>0);
	   if(Platform.Pos(L"D64")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_64"][i]->AsBoolean=!IsDesignOnly(CompleteFileName);
	   if(Platform.Pos(L"DAD")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_AD"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"DA6")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_A6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"DIO")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_IO"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"DI6")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_I6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"DOS")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_OS"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"DO6")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_O6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	  }
	else if(p==4)
	  {
	   if(Platform.Pos(L"C32")>0)
		 NG->CellByName[Name+L"_32"][i]->AsBoolean=true;
	   if(Platform.Pos(L"C64")>0)
		 NG->CellByName[Name+L"_64"][i]->AsBoolean=true;
	   if(Platform.Pos(L"CAD")>0)
		 NG->CellByName[Name+L"_AD"][i]->AsBoolean=true;
	   if(Platform.Pos(L"CA6")>0)
		 NG->CellByName[Name+L"_A6"][i]->AsBoolean=true;
	   if(Platform.Pos(L"CIO")>0)
		 NG->CellByName[Name+L"_IO"][i]->AsBoolean=true;
	   if(Platform.Pos(L"CI6")>0)
		 NG->CellByName[Name+L"_I6"][i]->AsBoolean=true;
	   if(Platform.Pos(L"COS")>0)
		 NG->CellByName[Name+L"_OS"][i]->AsBoolean=true;
	   if(Platform.Pos(L"CO6")>0)
		 NG->CellByName[Name+L"_O6"][i]->AsBoolean=true;
	  }
	else if(p<7)
	  {
	   if(Name==L"C6")
		 NG->CellByName[Name+L"_32"][i]->AsBoolean=true;
	  }
	else if(p==7)
	  {
	   if(Platform.Pos(L"C32")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_32"][i]->AsBoolean=true;
	   if(NG->CellByName[Name+L"_32"][i]->AsBoolean&&Name.SubString(3,2)!=L"XE")
		 NG->CellByName[Name+L"_32"][i]->AsBoolean=!(IsFMX(CompleteFileName)>0);
	   if(Platform.Pos(L"C64")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_64"][i]->AsBoolean=!IsDesignOnly(CompleteFileName);
	   if(Platform.Pos(L"CAD")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_AD"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"CA6")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_A6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"CIO")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_IO"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"CI6")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_I6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"COS")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_OS"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	   if(Platform.Pos(L"CO6")>0&&Name.SubString(1,2)==L"RS")
		 NG->CellByName[Name+L"_O6"][i]->AsBoolean=(IsFMX(CompleteFileName)>0|IsNoVCL(CompleteFileName)<1)&(!IsDesignOnly(CompleteFileName));
	  }
   }
 NG->BestFitRow(i);
// NG->BestFitColumns(bfBoth,true);
 FileList->Add(CompleteFileName);
 return i;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnDeleteClick(TObject *Sender)
{
 if(NG->SelectedRow<0)
   return;
 UnicodeString Str,FileName=NG->CellByName["NGFile"][NG->SelectedRow]->AsString;
 Str=_(L"你确定要删除")+FileName+_(L"？");
 if(Application->MessageBox(Str.c_str(),_(L"删除").c_str(),MB_YESNO+MB_ICONQUESTION+MB_TOPMOST)==IDNO)
   return;
 NG->DeleteRow(NG->SelectedRow);
 if(CfgName!=L"")
   FileName=GetCompleteFileName(ExtractFilePath(CfgName),FileName);
 FileList->Delete(FileList->IndexOf(FileName));
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GetEnvVar(TQJson *QJson)
{
 UnicodeString RootDir,Type,BDS=L"",Common=L"",Boost=L"",Boost64=L"",Str;
 RootDir=QJson->ForcePath("RootDir")->AsString;
 if(RootDir==L"")
   return;
 Type=QJson->ForcePath("Type")->AsString;
 if(Type!=L"BDS")
   {
	QJson->ForcePath("CommonDir")->AsString=RootDir+L"\\Projects";
	return;
   }
 if(!FileExists(RootDir+L"\\Bin\\RSVars.bat"))
   {
	return;
   }
 TStringList *EnvList=new TStringList();
 EnvList->LoadFromFile(RootDir+L"\\Bin\\RSVars.bat");
 for(int i=0;i<EnvList->Count;i++)
   {
	Str=EnvList->Strings[i];
	if(Str.LowerCase().Pos(L"bds=")>0)
	  {
	   BDS=ExcludeTrailingPathDelimiter(EnvList->ValueFromIndex[i]);
	  }
	else if(Str.LowerCase().Pos(L"bdscommondir=")>0)
	  {
	   Common=ExcludeTrailingPathDelimiter(EnvList->ValueFromIndex[i]);
	  }
	else if(Str.LowerCase().Pos(L"cg_boost_root=")>0)
	  {
	   Boost=ExcludeTrailingPathDelimiter(EnvList->ValueFromIndex[i]);
	  }
	else if(Str.LowerCase().Pos(L"cg_64_boost_root=")>0)
	  {
	   Boost64=ExcludeTrailingPathDelimiter(EnvList->ValueFromIndex[i]);
	  }
   }
 if(Common!=L"")
   {
	if(DirectoryExists(Common))
	  QJson->ForcePath("CommonDir")->AsString=Common;
   }
 if(Boost==L"")
   {
	  Boost=RootDir+L"\\Include\\boost_1_39";
   }
 else if(Boost.LowerCase().Pos(BDS.LowerCase())>0)
   {
	  Boost=Boost.SubString(BDS.Length()+1,Boost.Length()-BDS.Length());
	  Boost=RootDir+Boost;
   }
 if(DirectoryExists(Boost))
   QJson->ForcePath("BoostDir")->AsString=Boost;
 if(Boost64==L"")
   {
	  Boost64=RootDir+L"\\Include\\boost_1_50";
   }
 else if(Boost64.LowerCase().Pos(BDS.LowerCase())>0)
   {
	  Boost64=Boost64.SubString(BDS.Length()+1,Boost64.Length()-BDS.Length());
	  Boost64=RootDir+Boost64;
   }
 if(DirectoryExists(Boost64))
   QJson->ForcePath("Boost64Dir")->AsString=Boost64;
 delete EnvList;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::Button1Click(TObject *Sender)
{
 UnicodeString Directory;
 Directory=FolderBrowser(OutFinalDir->Text);
 if(Directory!=L"")
   OutFinalDir->Text=Directory;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::Button2Click(TObject *Sender)
{
 UnicodeString Directory;
 Directory=FolderBrowser(OutIncludeDir->Text);
 if(Directory!=L"")
   OutIncludeDir->Text=Directory;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::Button3Click(TObject *Sender)
{
 UnicodeString Directory;
 Directory=FolderBrowser(OutLibDir->Text);
 if(Directory!=L"")
   OutLibDir->Text=Directory;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::Button5Click(TObject *Sender)
{
 TForm_PathSetup *Form=new TForm_PathSetup(this);
 Form->Init(ExtraIncludeDir->Text);
 Form->ShowModal();
 if(Form->BtnSave->Tag==1)
   {
	ExtraIncludeDir->Text=Form->GetPaths();
   }
 delete Form;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::Button4Click(TObject *Sender)
{
 TForm_PathSetup *Form=new TForm_PathSetup(this);
 Form->Init(ExtraLibDir->Text);
 Form->ShowModal();
 if(Form->BtnSave->Tag==1)
   {
	  ExtraLibDir->Text=Form->GetPaths();
   }
 delete Form;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::Button6Click(TObject *Sender)
{
 TForm_PathSetup *Form=new TForm_PathSetup(this);
 Form->Init(ExtraSearchDir->Text);
 Form->ShowModal();
 if(Form->BtnSave->Tag==1)
   {
	ExtraSearchDir->Text=Form->GetPaths();
   }
 delete Form;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnSaveClick(TObject *Sender)
{
 if(CfgName==L"")
   {
	SD->DefaultExt=L"CompilerCfg";
	SD->Filter=_(L"配置文件")+L"|*.CompilerCfg";
	SD->Title=_(L"选择保存配置文件");
	if(SD->Execute())
	  {
	   CfgName=SD->FileName;
	   SaveProject(CfgName);
	  }
   }
 else
   SaveProject(CfgName);
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnSaveAsClick(TObject *Sender)
{
 UnicodeString FileName;
 SD->DefaultExt=L"CompilerCfg";
 SD->Filter=_(L"配置文件")+L"|*.CompilerCfg";
 SD->Title=_(L"选择保存配置文件");
 if(CfgName!=L"")
   {
	  SD->InitialDir=ExtractFilePath(CfgName);
	  SD->FileName=CfgName;
   }
 if(SD->Execute())
   {
	  FileName=SD->FileName;
   }
 if(FileName==L"")
   return;
 CfgName=FileName;
 SaveProject(CfgName);
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::SaveProject(UnicodeString FileName)
{
 UnicodeString Name,Platform;
 TQJson *QJson=new TQJson();
 QJson->ForcePath("Ver")->AsInteger=2;
 QJson->ForcePath("UpdateTime")->AsDateTime=Now();
 if(OutFinalDir->Text==L"")
   OutFinalDir->Text=L".\\Bin";
 if(OutIncludeDir->Text==L"")
   OutIncludeDir->Text=L".\\Include";
 if(OutLibDir->Text==L"")
   OutLibDir->Text=L".\\Lib";
 QJson->ForcePath("Project.Output.Bin")->AsString=OutFinalDir->Text;
 QJson->ForcePath("Project.Output.Include")->AsString=OutIncludeDir->Text;
 QJson->ForcePath("Project.Output.Lib")->AsString=OutLibDir->Text;
 QJson->ForcePath("Project.Extra.Alias")->AsString=ExtraAlias->Text;
 QJson->ForcePath("Project.Extra.NameSpaceSearch")->AsString=ExtraNameSpaceSearch->Text;
 QJson->ForcePath("Project.Extra.Include")->AsString=ExtraIncludeDir->Text;
 QJson->ForcePath("Project.Extra.Lib")->AsString=ExtraLibDir->Text;
 QJson->ForcePath("Project.Extra.Search")->AsString=ExtraSearchDir->Text;
 QJson->ForcePath("Project.Extra.NameSpaces")->AsBoolean=ExtraNameSpaces->Checked;
 QJson->ForcePath("Project.Extra.BuildAllUnits")->AsBoolean=ExtraBuildAllUnits->Checked;
 QJson->ForcePath("Project.Extra.DpkAddIDEVersion")->AsBoolean=DpkAddIDEVersion->Checked;
 for(int i=0;i<NG->RowCount;i++)
   {
	  QJson->ForcePath("Project.Files["+IntToStr(i)+L"].Use")->AsBoolean=NG->CellByName["NGUse"][i]->AsBoolean;
	  QJson->ForcePath("Project.Files["+IntToStr(i)+L"].FileName")->AsString=NG->CellByName["NGFile"][i]->AsString;
	  for(int j=0;j<NG->Columns->Count;j++)
	    {
	     if(NG->Columns->Item[j]->ClassNameIs(L"TNxCheckBoxColumn")&&NG->Columns->Item[j]->Name!=L"NGUse")
		     {
		      Name=NG->Columns->Item[j]->Name;
		      QJson->ForcePath("Project.Files["+IntToStr(i)+L"].Compile."+Name)->AsBoolean=NG->CellByName[Name][i]->AsBoolean;
         }
	    }
   }
 QJson->SaveToFile(FileName,teUTF8,true);
 delete QJson;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::LoadProject(UnicodeString FileName)
{
 UnicodeString Name,Platform,TmpStr;
 int p;
 TQJson *QJson=new TQJson();
 QJson->LoadFromFile(FileName,teUTF8);
 NG->ClearRows();
 if(!QJson->ForcePath("Ver")->IsNumeric)
   {
	  OutIncludeDir->Text=QJson->ForcePath("Project.OutPath.Include")->AsString;
	  OutLibDir->Text=QJson->ForcePath("Project.OutPath.Lib")->AsString;
	  ExtraIncludeDir->Text=QJson->ForcePath("Project.ExtPath.Include")->AsString;
	  ExtraLibDir->Text=QJson->ForcePath("Project.ExtPath.Lib")->AsString;
	  ExtraSearchDir->Text=QJson->ForcePath("Project.ExtPath.Search")->AsString;
	  ExtraAlias->Text=QJson->ForcePath("Project.Alias")->AsString;
	  for(int i=0;i<QJson->ForcePath("Project.Files")->Count;i++)
	    AddFile(QJson->ForcePath("Project.Files["+IntToStr(i)+L"]")->AsString);
   }
 else
   {
	  OutFinalDir->Text=QJson->ForcePath("Project.Output.Bin")->AsString;
	  OutIncludeDir->Text=QJson->ForcePath("Project.Output.Include")->AsString;
	  OutLibDir->Text=QJson->ForcePath("Project.Output.Lib")->AsString;
	  ExtraAlias->Text=QJson->ForcePath("Project.Extra.Alias")->AsString;
	  ExtraNameSpaceSearch->Text=QJson->ForcePath("Project.Extra.NameSpaceSearch")->AsString;
	  ExtraIncludeDir->Text=QJson->ForcePath("Project.Extra.Include")->AsString;
	  ExtraLibDir->Text=QJson->ForcePath("Project.Extra.Lib")->AsString;
	  ExtraSearchDir->Text=QJson->ForcePath("Project.Extra.Search")->AsString;
	  if(QJson->ForcePath("Project.Extra")->IndexOf(L"NameSpaces")>=0)
		ExtraNameSpaces->Checked=QJson->ForcePath("Project.Extra.NameSpaces")->AsBoolean;
	  else
		ExtraNameSpaces->Checked=true;
	  if(QJson->ForcePath("Project.Extra")->IndexOf(L"BuildAllUnits")>=0)
		ExtraBuildAllUnits->Checked=QJson->ForcePath("Project.Extra.BuildAllUnits")->AsBoolean;
	  else
		ExtraBuildAllUnits->Checked=false;
	  if(QJson->ForcePath("Project.Extra")->IndexOf(L"DpkAddIDEVersion")>=0)
	    DpkAddIDEVersion->Checked=QJson->ForcePath("Project.Extra.DpkAddIDEVersion")->AsBoolean;
	  for(int i=0;i<QJson->ForcePath("Project.Files")->Count;i++)
		{
		 TmpStr=QJson->ForcePath("Project.Files["+IntToStr(i)+"].FileName")->AsString;
		 p=AddFile(TmpStr);
		 NG->CellByName["NGUse"][p]->AsBoolean=QJson->ForcePath("Project.Files["+IntToStr(i)+"].Use")->AsBoolean;
	     for(int j=0;j<NG->Columns->Count;j++)
		     {
		      if(NG->Columns->Item[j]->ClassNameIs(L"TNxCheckBoxColumn")&&NG->Columns->Item[j]->Name!=L"NGUse")
			      {
			       Name=NG->Columns->Item[j]->Name;
			       NG->CellByName[Name][i]->AsBoolean=QJson->BoolByPath("Project.Files["+IntToStr(i)+"].Compile."+Name,false);
			      }
		    }
	    }
   }
 delete QJson;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::Button7Click(TObject *Sender)
{
 OD->DefaultExt=L"exe";
 OD->FileName=L"";
 OD->Filter=_(L"所有可用文件")+L"|bds.exe;bcb.exe;delphi32.exe;dcc32.exe;dcc64.exe|程序主文件|bds.exe;bcb.exe;delphi32.exe|Delphi编译程序|dcc32.exe;dcc64.exe";
 OD->Title=_(L"请选择要打开的文件");
 UnicodeString RootDir,App;
 if(OD->Execute())
   {
	  App=OD->FileName;
	  RootDir=ExcludeTrailingPathDelimiter(ExtractFilePath(ExcludeTrailingPathDelimiter(ExtractFilePath(App))));
	  TQJson *QJson=new TQJson();
	  QJson->ForcePath("RootDir")->AsString=RootDir;
	  CheckCompiler(QJson);
	  if(QJson->ForcePath("RootDir")->AsString!=L"")
		{
		 L->Caption=QJson->ForcePath("Name")->AsString;
		 CustomRootDir->Text=QJson->ForcePath("RootDir")->AsString;
		}
	  delete QJson;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::AddIDEClick(TObject *Sender)
{
 if(CustomRootDir->Text==L"")
   {
	  Application->MessageBox(_(L"请先设置主目录！").c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	  CustomRootDir->SetFocus();
	return;
   }
 TQJson *QJson=new TQJson();
 QJson->ForcePath("RootDir")->AsString=CustomRootDir->Text;
 CheckCompiler(QJson);
 if(QJson->ForcePath("RootDir")->AsString==L"")
   {
	delete QJson;
	  return;
   }
 if(CompilerList->IndexOfName(QJson->ForcePath("Ver")->AsString)<0)
   {
	  int Count;
	CustomCfg->ForcePath("Custom.UpdateTime")->AsDateTime=Now();
	if(CustomCfg->ForcePath("Custom")->IndexOf(L"Compiles")>=0)
	  {
	   if(CustomCfg->ForcePath("Custom.Compiles")->IsArray)
		 Count=CustomCfg->ForcePath("Custom.Compiles")->Count;
	   else
		 {
		  Count=0;
		  CustomCfg->ForcePath("Custom")->Delete(CustomCfg->ForcePath("Custom")->IndexOf(L"Compiles"));
		  CustomCfg->ForcePath("Custom")->AddArray(L"Compiles");
		 }
	  }
	else
	  {
	   Count=0;
	   CustomCfg->ForcePath("Custom")->AddArray(L"Compiles");
	  }
	CustomCfg->ForcePath("Custom.Compiles["+IntToStr(Count)+L"].RootDir")->AsString=QJson->ForcePath("RootDir")->AsString;
	CustomCfg->SaveToFile(ExtractFilePath(Application->ExeName)+L"Data.Cfg",teUTF8,true);
	CompilerList->AddObject(QJson->ForcePath("Ver")->AsString+L"="+QJson->ForcePath("Platform")->AsString,QJson);
	CreateTabSheet(QJson);
	if(QJson->ForcePath("Platform")->AsString.Pos(L"32")>0)
		{
	   TNxCheckBoxColumn *NCBC32=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBC32->Name=QJson->ForcePath("Ver")->AsString+L"_32";
	   NCBC32->Header->Caption=QJson->ForcePath("Ver")->AsString+_(L" 32位");
	   NCBC32->Header->Alignment=taCenter;
	   NCBC32->Alignment=taCenter;
	   NCBC32->Width=88;
	   NCBC32->Options>>coCanSort;
	   SetColumnByExt(QJson->ForcePath("Ver")->AsString+L"_32",QJson->ForcePath("Platform")->AsString);
	  }
	if(QJson->ForcePath("Platform")->AsString.Pos(L"64")>0)
	  {
	   TNxCheckBoxColumn *NCBC64=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBC64->Name=QJson->ForcePath("Ver")->AsString+L"_64";
	   NCBC64->Header->Caption=QJson->ForcePath("Ver")->AsString+_(L" 64位");
	   NCBC64->Header->Alignment=taCenter;
	   NCBC64->Alignment=taCenter;
	   NCBC64->Width=88;
	   NCBC64->Options>>coCanSort;
	   SetColumnByExt(QJson->ForcePath("Ver")->AsString+L"_64",QJson->ForcePath("Platform")->AsString);
	  }
	if(QJson->ForcePath("Platform")->AsString.Pos(L"AD")>0)
  	  {
  	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
  	   NCBCAD->Name=QJson->ForcePath("Ver")->AsString+L"_AD";
	   NCBCAD->Header->Caption=QJson->ForcePath("Ver")->AsString+_(L" 安卓");
  	   NCBCAD->Header->Alignment=taCenter;
  	   NCBCAD->Alignment=taCenter;
  	   NCBCAD->Width=88;
  	   NCBCAD->Options>>coCanSort;
  	   SetColumnByExt(QJson->ForcePath("Ver")->AsString+L"_AD",QJson->ForcePath("Platform")->AsString);
  	  }
	if(QJson->ForcePath("Platform")->AsString.Pos(L"A6")>0)
  	  {
	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBCAD->Name=QJson->ForcePath("Ver")->AsString+L"_A6";
	   NCBCAD->Header->Caption=QJson->ForcePath("Ver")->AsString+_(L" 安卓64");
  	   NCBCAD->Header->Alignment=taCenter;
  	   NCBCAD->Alignment=taCenter;
  	   NCBCAD->Width=88;
  	   NCBCAD->Options>>coCanSort;
  	   SetColumnByExt(QJson->ForcePath("Ver")->AsString+L"_A6",QJson->ForcePath("Platform")->AsString);
  	  }
	if(QJson->ForcePath("Platform")->AsString.Pos(L"IO")>0)
  	  {
  	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
  	   NCBCAD->Name=QJson->ForcePath("Ver")->AsString+L"_IO";
	   NCBCAD->Header->Caption=QJson->ForcePath("Ver")->AsString+_(L" IOS");
  	   NCBCAD->Header->Alignment=taCenter;
  	   NCBCAD->Alignment=taCenter;
  	   NCBCAD->Width=88;
  	   NCBCAD->Options>>coCanSort;
  	   SetColumnByExt(QJson->ForcePath("Ver")->AsString+L"_IO",QJson->ForcePath("Platform")->AsString);
  	  }
   	if(QJson->ForcePath("Platform")->AsString.Pos(L"I6")>0)
  	  {
  	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
  	   NCBCAD->Name=QJson->ForcePath("Ver")->AsString+L"_I6";
	   NCBCAD->Header->Caption=QJson->ForcePath("Ver")->AsString+_(L" IOS64");
  	   NCBCAD->Header->Alignment=taCenter;
  	   NCBCAD->Alignment=taCenter;
  	   NCBCAD->Width=88;
  	   NCBCAD->Options>>coCanSort;
  	   SetColumnByExt(QJson->ForcePath("Ver")->AsString+L"_I6",QJson->ForcePath("Platform")->AsString);
  	  }
  	if(QJson->ForcePath("Platform")->AsString.Pos(L"OS")>0)
  	  {
  	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
  	   NCBCAD->Name=QJson->ForcePath("Ver")->AsString+L"_OS";
	   NCBCAD->Header->Caption=QJson->ForcePath("Ver")->AsString+_(L" OSX");
  	   NCBCAD->Header->Alignment=taCenter;
  	   NCBCAD->Alignment=taCenter;
  	   NCBCAD->Width=88;
  	   NCBCAD->Options>>coCanSort;
  	   SetColumnByExt(QJson->ForcePath("Ver")->AsString+L"_OS",QJson->ForcePath("Platform")->AsString);
  	  }
	if(QJson->ForcePath("Platform")->AsString.Pos(L"O6")>0)
  	  {
  	   TNxCheckBoxColumn *NCBCAD=(TNxCheckBoxColumn *)NG->Columns->Add(__classid(TNxCheckBoxColumn));
	   NCBCAD->Name=QJson->ForcePath("Ver")->AsString+L"_O6";
	   NCBCAD->Header->Caption=QJson->ForcePath("Ver")->AsString+_(L" OSX64");
  	   NCBCAD->Header->Alignment=taCenter;
  	   NCBCAD->Alignment=taCenter;
  	   NCBCAD->Width=88;
  	   NCBCAD->Options>>coCanSort;
  	   SetColumnByExt(QJson->ForcePath("Ver")->AsString+L"_O6",QJson->ForcePath("Platform")->AsString);
  	  }
	CustomCfg->SaveToFile(ExtractFilePath(Application->ExeName)+L"Data.Cfg",teUTF8,true);
   }
 else
   {
	Application->MessageBox(_(L"此版本已经存在，不能再次添加！").c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	  delete QJson;
   }
 NG->Update();
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GetOnlyFromConfig()
{
 if(FileExists(ExtractFilePath(Application->ExeName)+L"Data.Cfg"))
   {
	TQJson *QJson=new TQJson();
	QJson->LoadFromFile(ExtractFilePath(Application->ExeName)+L"Data.Cfg",teUTF8);
	if(QJson->IndexOf(L"OnlyFromConfig")>=0)
	  OnlyFromConfig=QJson->ForcePath(L"OnlyFromConfig")->AsBoolean;
	if(QJson->IndexOf(L"LangID")>=0)
	  SelectLangID=QJson->ForcePath(L"LangID")->AsInteger;
	delete QJson;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GetCompilerFromConfig()
{
 if(FileExists(ExtractFilePath(Application->ExeName)+L"Data.Cfg"))
   {
	  CustomCfg->LoadFromFile(ExtractFilePath(Application->ExeName)+L"Data.Cfg",teUTF8);
	  for(int i=0;i<CustomCfg->ForcePath("Custom.Compiles")->Count;i++)
	    {
	     TQJson *QJson=new TQJson();
	     QJson->ForcePath("RootDir")->AsString=GetCompleteFileName(ExtractFilePath(Application->ExeName),CustomCfg->ForcePath("Custom.Compiles["+IntToStr(i)+L"].RootDir")->AsString);
	     CheckCompiler(QJson);
	     if(QJson->ForcePath("RootDir")->AsString==L"")
		     {
		      delete QJson;
		     }
	     else
		     {
		      if(CompilerList->IndexOfName(QJson->ForcePath("Ver")->AsString)<0)
			      {
			       CompilerList->AddObject(QJson->ForcePath("Ver")->AsString+L"="+QJson->ForcePath("Platform")->AsString,QJson);
			       CreateTabSheet(QJson);
			      }
		      else
			      {
			       delete QJson;
            }
         }
      }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::SetColumnByExt(UnicodeString ColumnName,UnicodeString Platform)
{
 UnicodeString ExtName;
 int p;
 for(int i=0;i<NG->RowCount;i++)
   {
	ExtName=ExtractFileExt(NG->CellByName["NGFile"][i]->AsString);
	p=ExtList->IndexOf(ExtName);
	if(p<3)
	  {
	   if(Platform.Pos(L"D32")>0||Platform.Pos(L"D64")>0||Platform.Pos(L"DAD")>0||Platform.Pos(L"DA6")>0||Platform.Pos(L"DIO")>0||Platform.Pos(L"DI6")>0||Platform.Pos(L"DOS")>0||Platform.Pos(L"DO6")>0)
		 NG->CellByName[ColumnName][i]->AsBoolean=true;
	  }
	else if(p==3)
	  {
	   if((Platform.Pos(L"D32")>0||Platform.Pos(L"D64")>0||Platform.Pos(L"DAD")>0||Platform.Pos(L"DA6")>0||Platform.Pos(L"DIO")>0||Platform.Pos(L"DI6")>0||Platform.Pos(L"DOS")>0||Platform.Pos(L"DO6")>0)&&ColumnName.SubString(1,2)==L"RS")
		 NG->CellByName[ColumnName][i]->AsBoolean=true;
	  }
	else if(p==4)
	  {
	   if(Platform.Pos(L"C32")>0||Platform.Pos(L"C64")>0||Platform.Pos(L"CAD")>0||Platform.Pos(L"CA6")>0||Platform.Pos(L"CIO")>0||Platform.Pos(L"CI6")>0||Platform.Pos(L"COS")>0||Platform.Pos(L"CO6")>0)
		 NG->CellByName[ColumnName][i]->AsBoolean=true;
	  }
	else if(p<7)
	  {
	   if(Platform.Pos(L"C32")>0&&ColumnName.SubString(1,2)==L"C6")
		 NG->CellByName[ColumnName][i]->AsBoolean=true;
	  }
	else if(p==7)
	  {
	   if((Platform.Pos(L"C32")>0||Platform.Pos(L"C64")>0||Platform.Pos(L"CAD")>0||Platform.Pos(L"CA6")>0||Platform.Pos(L"CIO")>0||Platform.Pos(L"CI6")>0||Platform.Pos(L"COS")>0||Platform.Pos(L"CO6")>0)&&ColumnName.SubString(1,2)==L"RS")
		 NG->CellByName[ColumnName][i]->AsBoolean=true;
	  }
	else
	  NG->CellByName[ColumnName][i]->AsBoolean=false;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::NGMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift,
          int X, int Y)
{
 if(NG->RowCount<=0)
   return;
 if(Button!=mbRight)
   return;
 int pos=NG->GetRowAtPos(X,Y);
 TPoint P;
 P.X=X;
 P.Y=Y;
 if(NG->GetColumnAtPos(P)->ClassNameIs(L"TNxCheckBoxColumn"))
   {
	PM_Column->Enabled=true;
	PM_SelectColumn->Tag=X;
	PM_ClearColumn->Tag=Y;
   }
 else
   {
	PM_Column->Enabled=false;
   }
 if(pos<0)
   {
	PM_Row->Enabled=false;
   }
 else
   {
	PM_Row->Enabled=true;
	PM_SelectRow->Tag=pos;
   }
 P=Mouse->CursorPos;
 PM->Popup(P.x,P.y);
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_SelectAllClick(TObject *Sender)
{
 for(int i=0;i<NG->RowCount;i++)
   {
	for(int j=0;j<NG->Columns->Count;j++)
	  {
	   if(NG->Columns->Item[j]->ClassNameIs(L"TNxCheckBoxColumn"))
		 {
		  NG->Cell[j][i]->AsBoolean=true;
		 }
	  }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_ClearAllClick(TObject *Sender)
{
 for(int i=0;i<NG->RowCount;i++)
   {
	for(int j=0;j<NG->Columns->Count;j++)
	  {
	   if(NG->Columns->Item[j]->ClassNameIs(L"TNxCheckBoxColumn"))
		 {
		  NG->Cell[j][i]->AsBoolean=false;
		 }
	  }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_SelectColumnClick(TObject *Sender)
{
 int X,Y;
 X=PM_SelectColumn->Tag;
 Y=PM_ClearColumn->Tag;
 TPoint P;
 P.X=X;
 P.Y=Y;
 UnicodeString ColumnName=NG->GetColumnAtPos(P)->Name;
 for(int i=0;i<NG->RowCount;i++)
   {
	NG->CellByName[ColumnName][i]->AsBoolean=true;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_ClearColumnClick(TObject *Sender)
{
 int X,Y;
 X=PM_SelectColumn->Tag;
 Y=PM_ClearColumn->Tag;
 TPoint P;
 P.X=X;
 P.Y=Y;
 UnicodeString ColumnName=NG->GetColumnAtPos(P)->Name;
 for(int i=0;i<NG->RowCount;i++)
   {
	NG->CellByName[ColumnName][i]->AsBoolean=false;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_DeSelectAllClick(TObject *Sender)
{
 for(int i=0;i<NG->RowCount;i++)
   {
	for(int j=0;j<NG->Columns->Count;j++)
	  {
	   if(NG->Columns->Item[j]->ClassNameIs(L"TNxCheckBoxColumn"))
		 {
		  NG->Cell[j][i]->AsBoolean=!NG->Cell[j][i]->AsBoolean;
		 }
	  }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_DeSelectColumnClick(TObject *Sender)
{
 int X,Y;
 X=PM_SelectColumn->Tag;
 Y=PM_ClearColumn->Tag;
 TPoint P;
 P.X=X;
 P.Y=Y;
 UnicodeString ColumnName=NG->GetColumnAtPos(P)->Name;
 for(int i=0;i<NG->RowCount;i++)
   {
	  NG->CellByName[ColumnName][i]->AsBoolean=!NG->CellByName[ColumnName][i]->AsBoolean;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_SelectRowClick(TObject *Sender)
{
 int Pos;
 Pos=PM_SelectRow->Tag;
 for(int i=0;i<NG->Columns->Count;i++)
   {
	if(NG->Columns->Item[i]->ClassNameIs(L"TNxCheckBoxColumn"))
	  NG->Cell[i][Pos]->AsBoolean=true;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_ClearRowClick(TObject *Sender)
{
 int Pos;
 Pos=PM_SelectRow->Tag;
 for(int i=0;i<NG->Columns->Count;i++)
   {
	if(NG->Columns->Item[i]->ClassNameIs(L"TNxCheckBoxColumn"))
	  NG->Cell[i][Pos]->AsBoolean=false;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_DeSelectRowClick(TObject *Sender)
{
 int Pos;
 Pos=PM_SelectRow->Tag;
 for(int i=0;i<NG->Columns->Count;i++)
   {
	if(NG->Columns->Item[i]->ClassNameIs(L"TNxCheckBoxColumn"))
	  NG->Cell[i][Pos]->AsBoolean=!NG->Cell[i][Pos]->AsBoolean;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::CheckColumnByExt()
{
 if(NG->RowCount==0)
   return;
 if(CompilerList->Count==0)
   return;
 UnicodeString Name,Platform,ExtName,FileName;
 int p;
 for(int i=0;i<CompilerList->Count;i++)
   {
	Name=CompilerList->Names[i];
	Platform=CompilerList->ValueFromIndex[i];
	for(int j=0;j<NG->RowCount;j++)
	  {
	   FileName=GetCompleteFileName(ExtractFilePath(CfgName),NG->CellByName["NGFile"][j]->AsString);
	   ExtName=ExtractFileExt(NG->CellByName["NGFile"][j]->AsString);
	   p=ExtList->IndexOf(ExtName);
	   if(Platform.Pos(L"32")>0)
         NG->CellByName[Name+L"_32"][j]->Color=clWindow;
	   if(Platform.Pos(L"64")>0)
         NG->CellByName[Name+L"_64"][j]->Color=clWindow;
	   if(Platform.Pos(L"AD")>0)
		 NG->CellByName[Name+L"_AD"][j]->Color=clWindow;
	   if(Platform.Pos(L"A6")>0)
		 NG->CellByName[Name+L"_A6"][j]->Color=clWindow;
	   if(Platform.Pos(L"IO")>0)
		 NG->CellByName[Name+L"_IO"][j]->Color=clWindow;
	   if(Platform.Pos(L"I6")>0)
		 NG->CellByName[Name+L"_I6"][j]->Color=clWindow;
	   if(Platform.Pos(L"OS")>0)
		 NG->CellByName[Name+L"_OS"][j]->Color=clWindow;
	   if(Platform.Pos(L"O6")>0)
		 NG->CellByName[Name+L"_O6"][j]->Color=clWindow;
	   if(p<3)
		 {
		  if(Platform.Pos(L"D")<0)
			{
			 if(Platform.Pos(L"32")>0)
			   NG->CellByName[Name+L"_32"][j]->AsBoolean=false;
			 if(Platform.Pos(L"64")>0)
			   NG->CellByName[Name+L"_64"][j]->AsBoolean=false;
			 if(Platform.Pos(L"AD")>0)
			   NG->CellByName[Name+L"_AD"][j]->AsBoolean=false;
			 if(Platform.Pos(L"A6")>0)
			   NG->CellByName[Name+L"_A6"][j]->AsBoolean=false;
			 if(Platform.Pos(L"IO")>0)
			   NG->CellByName[Name+L"_IO"][j]->AsBoolean=false;
			 if(Platform.Pos(L"I6")>0)
			   NG->CellByName[Name+L"_I6"][j]->AsBoolean=false;
			 if(Platform.Pos(L"OS")>0)
			   NG->CellByName[Name+L"_OS"][j]->AsBoolean=false;
			 if(Platform.Pos(L"O6")>0)
			   NG->CellByName[Name+L"_O6"][j]->AsBoolean=false;
			}
		  if(p>0&&IsFMX(FileName)&&Name.Pos(L"XE")==0)
			{
			 if(NG->CellByName[Name+L"_32"][j]->AsBoolean)
			   NG->CellByName[Name+L"_32"][j]->AsBoolean=false;
			}
		  if(p>0&&Platform.Pos(L"64")>0)
			{
			 if(NG->CellByName[Name+L"_64"][j]->AsBoolean)
			   NG->CellByName[Name+L"_64"][j]->AsBoolean=!IsDesignOnly(FileName);
			}
		  if(p>0&&Platform.Pos(L"DAD")>0)
			{
			 if(NG->CellByName[Name+L"_AD"][j]->AsBoolean)
			   NG->CellByName[Name+L"_AD"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(p>0&&Platform.Pos(L"DA6")>0)
			{
			 if(NG->CellByName[Name+L"_A6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_A6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(p>0&&Platform.Pos(L"DIO")>0)
			{
			 if(NG->CellByName[Name+L"_IO"][j]->AsBoolean)
			   NG->CellByName[Name+L"_IO"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(p>0&&Platform.Pos(L"DI6")>0)
			{
			 if(NG->CellByName[Name+L"_I6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_I6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(p>0&&Platform.Pos(L"DOS")>0)
			{
			 if(NG->CellByName[Name+L"_OS"][j]->AsBoolean)
			   NG->CellByName[Name+L"_OS"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(p>0&&Platform.Pos(L"DO6")>0)
			{
			 if(NG->CellByName[Name+L"_O6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_O6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		 }
	   else if(p==3)
		 {
		  if(Platform.Pos(L"D")<0||Name.SubString(1,2)!=L"RS")
			{
			 if(Platform.Pos(L"32")>0)
			   NG->CellByName[Name+L"_32"][j]->AsBoolean=false;
			 if(Platform.Pos(L"64")>0)
			   NG->CellByName[Name+L"_64"][j]->AsBoolean=false;
			 if(Platform.Pos(L"AD")>0)
			   NG->CellByName[Name+L"_AD"][j]->AsBoolean=false;
			 if(Platform.Pos(L"A6")>0)
			   NG->CellByName[Name+L"_A6"][j]->AsBoolean=false;
			 if(Platform.Pos(L"IO")>0)
			   NG->CellByName[Name+L"_IO"][j]->AsBoolean=false;
			 if(Platform.Pos(L"I6")>0)
			   NG->CellByName[Name+L"_I6"][j]->AsBoolean=false;
			 if(Platform.Pos(L"OS")>0)
			   NG->CellByName[Name+L"_OS"][j]->AsBoolean=false;
			 if(Platform.Pos(L"O6")>0)
			   NG->CellByName[Name+L"_O6"][j]->AsBoolean=false;
			}
		  if(IsFMX(FileName)==1&&Name.Pos(L"XE")==0)
			{
			 if(NG->CellByName[Name+L"_32"][j]->AsBoolean)
			   NG->CellByName[Name+L"_32"][j]->AsBoolean=false;
			}
		  if(Platform.Pos(L"64")>0)
			{
			 if(NG->CellByName[Name+L"_64"][j]->AsBoolean)
			   NG->CellByName[Name+L"_64"][j]->AsBoolean=!IsDesignOnly(FileName);
			}
		  if(Platform.Pos(L"DAD")>0)
			{
			 if(NG->CellByName[Name+L"_AD"][j]->AsBoolean)
			   NG->CellByName[Name+L"_AD"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"DA6")>0)
			{
			 if(NG->CellByName[Name+L"_A6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_A6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"DIO")>0)
			{
			 if(NG->CellByName[Name+L"_IO"][j]->AsBoolean)
			   NG->CellByName[Name+L"_IO"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"DI6")>0)
			{
			 if(NG->CellByName[Name+L"_I6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_I6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"DOS")>0)
			{
			 if(NG->CellByName[Name+L"_OS"][j]->AsBoolean)
			   NG->CellByName[Name+L"_OS"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"DO6")>0)
			{
			 if(NG->CellByName[Name+L"_O6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_O6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		 }
	   else if(p==4)
		 {
		  if(Platform.Pos(L"C")<0)
			{
			 if(Platform.Pos(L"32")>0)
			   NG->CellByName[Name+L"_32"][j]->AsBoolean=false;
			 if(Platform.Pos(L"64")>0)
			   NG->CellByName[Name+L"_64"][j]->AsBoolean=false;
			 if(Platform.Pos(L"AD")>0)
			   NG->CellByName[Name+L"_AD"][j]->AsBoolean=false;
			 if(Platform.Pos(L"A6")>0)
			   NG->CellByName[Name+L"_A6"][j]->AsBoolean=false;
			 if(Platform.Pos(L"IO")>0)
			   NG->CellByName[Name+L"_IO"][j]->AsBoolean=false;
			 if(Platform.Pos(L"I6")>0)
			   NG->CellByName[Name+L"_I6"][j]->AsBoolean=false;
			 if(Platform.Pos(L"OS")>0)
			   NG->CellByName[Name+L"_OS"][j]->AsBoolean=false;
			 if(Platform.Pos(L"O6")>0)
			   NG->CellByName[Name+L"_O6"][j]->AsBoolean=false;
			}
		 }
	   else if(p<7)
		 {
		  if(Platform.Pos(L"C")<0||Name!=L"C6")
			{
			 if(Platform.Pos(L"32")>0)
			   NG->CellByName[Name+L"_32"][j]->AsBoolean=false;
			 if(Platform.Pos(L"64")>0)
			   NG->CellByName[Name+L"_64"][j]->AsBoolean=false;
			}
		  if(Platform.Pos(L"AD")>0)
			NG->CellByName[Name+L"_AD"][j]->AsBoolean=false;
		  if(Platform.Pos(L"A6")>0)
			NG->CellByName[Name+L"_A6"][j]->AsBoolean=false;
		  if(Platform.Pos(L"IO")>0)
			NG->CellByName[Name+L"_IO"][j]->AsBoolean=false;
		  if(Platform.Pos(L"I6")>0)
			NG->CellByName[Name+L"_I6"][j]->AsBoolean=false;
		  if(Platform.Pos(L"OS")>0)
			NG->CellByName[Name+L"_OS"][j]->AsBoolean=false;
		  if(Platform.Pos(L"O6")>0)
			NG->CellByName[Name+L"_O6"][j]->AsBoolean=false;
		 }
	   else if(p==7)
		 {
		  if(Platform.Pos(L"C")<0||Name.SubString(1,2)!=L"RS")
			{
			 if(Platform.Pos(L"32")>0)
			   NG->CellByName[Name+L"_32"][j]->AsBoolean=false;
			 if(Platform.Pos(L"64")>0)
			   NG->CellByName[Name+L"_64"][j]->AsBoolean=false;
			 if(Platform.Pos(L"AD")>0)
			   NG->CellByName[Name+L"_AD"][j]->AsBoolean=false;
			 if(Platform.Pos(L"A6")>0)
			   NG->CellByName[Name+L"_A6"][j]->AsBoolean=false;
			 if(Platform.Pos(L"IO")>0)
			   NG->CellByName[Name+L"_IO"][j]->AsBoolean=false;
			 if(Platform.Pos(L"I6")>0)
			   NG->CellByName[Name+L"_I6"][j]->AsBoolean=false;
			 if(Platform.Pos(L"OS")>0)
			   NG->CellByName[Name+L"_OS"][j]->AsBoolean=false;
			 if(Platform.Pos(L"O6")>0)
			   NG->CellByName[Name+L"_O6"][j]->AsBoolean=false;
			}
		  if(IsFMX(FileName)&&Name.Pos(L"XE")==0)
			{
			 if(NG->CellByName[Name+L"_32"][j]->AsBoolean)
			   NG->CellByName[Name+L"_32"][j]->AsBoolean=false;
			}
		  if(Platform.Pos(L"64")>0)
			{
			 if(NG->CellByName[Name+L"_64"][j]->AsBoolean)
			   NG->CellByName[Name+L"_64"][j]->AsBoolean=!IsDesignOnly(FileName);
			}
		  if(Platform.Pos(L"CAD")>0)
			{
			 if(NG->CellByName[Name+L"_AD"][j]->AsBoolean)
			   NG->CellByName[Name+L"_AD"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"CA6")>0)
			{
			 if(NG->CellByName[Name+L"_A6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_A6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"CIO")>0)
			{
			 if(NG->CellByName[Name+L"_IO"][j]->AsBoolean)
			   NG->CellByName[Name+L"_IO"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"CI6")>0)
			{
			 if(NG->CellByName[Name+L"_I6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_I6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"COS")>0)
			{
			 if(NG->CellByName[Name+L"_OS"][j]->AsBoolean)
			   NG->CellByName[Name+L"_OS"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		  if(Platform.Pos(L"CO6")>0)
			{
			 if(NG->CellByName[Name+L"_O6"][j]->AsBoolean)
			   NG->CellByName[Name+L"_O6"][j]->AsBoolean=(IsFMX(FileName)>0|IsNoVCL(FileName)<1)&(!IsDesignOnly(FileName));
			}
		 }
	  }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnCompilerClick(TObject *Sender)
{
/* TODO : BtnCompilerClick */
 if(BtnCompiler->Tag==1)
   {
	JobGroup->Cancel();
	BtnCompiler->Tag=0;
	P1->Enabled=true;
	BtnCompiler->Caption=_(L"编译");
	return;
   }
 if(NG->RowCount==0)
   {
	Application->MessageBox(_(L"至少要添加一个文件！").c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	BtnAdd->SetFocus();
	return;
   }
 if(OutFinalDir->Text==L"")
   {
	Application->MessageBox(_(L"必须要选择一个Bin目录！").c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	OutFinalDir->SetFocus();
	return;
   }
 if(OutIncludeDir->Text==L"")
   {
	Application->MessageBox(_(L"必须要选择一个Include目录！").c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	OutIncludeDir->SetFocus();
	return;
   }
 if(OutLibDir->Text==L"")
   {
	Application->MessageBox(_(L"必须要选择一个Lib目录！").c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	OutLibDir->SetFocus();
	return;
   }
 UnicodeString Name,Platform,PlatformName;
// CheckColumnByExt();
 P1->Enabled=false;
 BtnCompiler->Tag=1;
 BtnCompiler->Caption=_(L"停止");
 try
   {
	try
	  {
	   if(JobGroup->MsgWaitFor()!=System::Types::wrSignaled)
		 Application->MessageBox(_(L"JobGroup出错！").c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	   JobGroup->Prepare();
	   for(int i=0;i<NG->Columns->Count;i++)
		 {
		  Name=NG->Columns->Item[i]->Name;
		  if(NG->Columns->Item[i]->ClassNameIs(L"TNxCheckBoxColumn")&&Name!=L"NGUse")
			{
			 Platform=Name.SubString(Name.Length()-1,2);
			 Name=Name.SubString(1,Name.Length()-3);
			 PlatformName=NG->Columns->Item[i]->Header->Caption;
			 for(int j=0;j<NG->RowCount;j++)
			   {
				if(NG->CellByName["NGUse"][j]->AsBoolean)
				  {
				   if(NG->Cell[i][j]->AsBoolean)
					 {
					  NG->Cell[i][j]->Color=clWindow;
					  TQJson *QJson=((TQJson *)CompilerList->Objects[CompilerList->IndexOfName(Name)])->Copy();
					  QJson->ForcePath("Project.PlatformName")->AsString=PlatformName;
					  QJson->ForcePath("Project.Platform")->AsString=Platform;
					  QJson->ForcePath("Project.GridFileName")->AsString=NG->CellByName["NGFile"][j]->AsString;
					  if(CfgName==L"")
						{
						 QJson->ForcePath("Project.FileName")->AsString=NG->CellByName["NGFile"][j]->AsString;
						 QJson->ForcePath("Project.OutFinalDir")->AsString=OutFinalDir->Text;
						 QJson->ForcePath("Project.OutIncludeDir")->AsString=OutIncludeDir->Text;
						 QJson->ForcePath("Project.OutLibDir")->AsString=OutLibDir->Text;
						}
					  else
						{
						 QJson->ForcePath("Project.FileName")->AsString=GetCompleteFileName(ExtractFilePath(CfgName),NG->CellByName["NGFile"][j]->AsString);
						 QJson->ForcePath("Project.OutFinalDir")->AsString=GetCompleteFileName(ExtractFilePath(CfgName),OutFinalDir->Text);
						 QJson->ForcePath("Project.OutIncludeDir")->AsString=GetCompleteFileName(ExtractFilePath(CfgName),OutIncludeDir->Text);
						 QJson->ForcePath("Project.OutLibDir")->AsString=GetCompleteFileName(ExtractFilePath(CfgName),OutLibDir->Text);
						}
					  QJson->ForcePath("Project.ExtraAlias")->AsString=ExtraAlias->Text;
					  QJson->ForcePath("Project.ExtraNameSpaceSearch")->AsString=ExtraNameSpaceSearch->Text;
					  QJson->ForcePath("Project.ExtraIncludeDir")->AsString=ReplPlatformPath(ExtraIncludeDir->Text,QJson);
					  QJson->ForcePath("Project.ExtraLibDir")->AsString=ReplPlatformPath(ExtraLibDir->Text,QJson);
					  QJson->ForcePath("Project.ExtraSearchDir")->AsString=ReplPlatformPath(ExtraSearchDir->Text,QJson);
					  QJson->ForcePath("Project.ShowCMD")->AsBoolean=ShowCmd->Checked;
					  QJson->ForcePath("Project.ShowAllLog")->AsBoolean=ShowAllLog->Checked;
					  QJson->ForcePath("Project.BuildDebug")->AsBoolean=BuildDebug->Checked;
					  QJson->ForcePath("Project.NameSpaces")->AsBoolean=ExtraNameSpaces->Checked;
					  QJson->ForcePath("Project.BuildAllUnits")->AsBoolean=ExtraBuildAllUnits->Checked;
					  QJson->ForcePath("Project.AddIDEVersion")->AsBoolean=DpkAddIDEVersion->Checked;

					  JobGroup->Add(CompilerJob,QJson,false,jdfFreeAsObject);
					 }
				  }
			   }
			}
		 }
	   JobGroup->Run();
     switch(JobGroup->MsgWaitFor())
       {
        case System::Types::wrAbandoned:
										Name=_(L"被中止");
										break;
		case System::Types::wrTimeout:
										Name=_(L"超时");
										break;
		case System::Types::wrError:
										Name=_(L"出错！");
										break;
		default:
										Name=_(L"完成");
	   }
	 SetCompilerResult(_(L"编译过程结束")+L"("+Name+L")"+_("！"));
	  }
	catch(Exception &e)
	  {
	   Name=_(L"Compiler出错,错误描述：")+e.Message;
	   Application->MessageBox(Name.c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	  }
   }
 __finally
   {
	P1->Enabled=true;
	BtnCompiler->Tag=0;
	BtnCompiler->Caption=_(L"编译");
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::ShowCompilerResult(PQJob AJob)
{
 wchar_t *s=(wchar_t *)AJob->Data;
 UnicodeString Str=s;
 Log->Lines->Add(Str);
 delete[] s;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::SetCompilerResult(UnicodeString Result)
{
 FullLogTxt+=Result+L"\r\n";
 wchar_t *s=new wchar_t[Result.Length()+1];
 wcscpy(s,Result.c_str());
 Workers->Post(ShowCompilerResult,s,true);
}
//---------------------------------------------------------------------------
/* TODO : GetPaths */
UnicodeString __fastcall TForm_Main::GetIncludePaths(TQJson *QJson)
{
 UnicodeString IncludeDirs,RootDir,Platform,BoostDir;
 int IDEVersion;
 RootDir=QJson->ForcePath("RootDir")->AsString;
 Platform=QJson->ForcePath("Project.Platform")->AsString;
 IDEVersion=QJson->ForcePath("IDEVersion")->AsInteger;
 if(IDEVersion>=15&&Platform==L"32")
   {
	IncludeDirs=RootDir+L"\\include\\windows\\crtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\windows\\sdk";
	IncludeDirs+=L";"+RootDir+L"\\include\\windows\\vcl";
	IncludeDirs+=L";"+RootDir+L"\\include\\windows\\rtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\windows\\fmx";
	IncludeDirs+=L";"+RootDir+L"\\include\\dinkumware";
	if(QJson->ForcePath("BoostDir")->AsString!=L"")
	  {
	   IncludeDirs+=L";"+QJson->ForcePath("BoostDir")->AsString;
	   IncludeDirs+=L";"+QJson->ForcePath("BoostDir")->AsString+"\\boost\\tr1\\tr1";
	  }
   }
 else if(IDEVersion>=15&&Platform==L"64")
   {
	IncludeDirs=RootDir+L"\\include\\windows\\crtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\windows\\sdk";
	IncludeDirs+=L";"+RootDir+L"\\include\\windows\\vcl";
	IncludeDirs+=L";"+RootDir+L"\\include\\windows\\rtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\windows\\fmx";
	IncludeDirs+=L";"+RootDir+L"\\include\\dinkumware64";
	if(QJson->ForcePath("BoostDir64")->AsString!=L"")
	  {
	   IncludeDirs+=L";"+QJson->ForcePath("BoostDir64")->AsString;
	   IncludeDirs+=L";"+QJson->ForcePath("BoostDir64")->AsString+"\\boost\\tr1\\tr1";
	  }
   }
 else if(IDEVersion>=19&&Platform==L"AD")
   {
	IncludeDirs=RootDir+L"\\include\\android\\crtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\android\\rtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\android\\fmx";
   }
 else if(IDEVersion>=23&&Platform==L"A6")
   {
	IncludeDirs=RootDir+L"\\include\\android\\crtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\android\\rtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\android\\fmx";
   }
 else if(IDEVersion>=19&&Platform==L"IO")
   {
	IncludeDirs=RootDir+L"\\include\\ios\\crtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\ios\\rtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\ios\\fmx";
   }
 else if(IDEVersion>=22&&Platform==L"I6")
   {
	IncludeDirs=RootDir+L"\\include\\ios\\crtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\ios\\rtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\ios\\fmx";
   }
 else if(IDEVersion>=17&&Platform==L"OS")
   {
	IncludeDirs=RootDir+L"\\include\\osx\\crtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\osx\\rtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\osx\\fmx";
   }
 else if(IDEVersion>=22&&Platform==L"O6")
   {
	IncludeDirs=RootDir+L"\\include\\osx\\crtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\osx\\rtl";
	IncludeDirs+=L";"+RootDir+L"\\include\\osx\\fmx";
   }
 else
   {
	IncludeDirs=RootDir+L"\\include";
	IncludeDirs+=L";"+RootDir+L"\\include\\vcl";
	IncludeDirs+=L";"+RootDir+L"\\dinkumware";
	IncludeDirs+=L";"+RootDir+L"\\Indy10";
	if(QJson->ForcePath("BoostDir")->AsString!=L"")
	  {
	   IncludeDirs+=L";"+QJson->ForcePath("BoostDir")->AsString;
	   IncludeDirs+=L";"+QJson->ForcePath("BoostDir")->AsString+"\\boost\\tr1\\tr1";
	  }
   }
 return IncludeDirs;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TForm_Main::GetLibPaths(TQJson *QJson,bool IsRelease)
{
 UnicodeString LibDirs,RootDir,Platform,BoostDir,Path;
 if(IsRelease)
   Path=L"Release";
 else
   Path=L"Debug";
 int IDEVersion;
 RootDir=QJson->ForcePath("RootDir")->AsString;
 Platform=QJson->ForcePath("Project.Platform")->AsString;
 IDEVersion=QJson->ForcePath("IDEVersion")->AsInteger;
 if(IDEVersion>=15&&Platform==L"32")
   {
	LibDirs=RootDir+L"\\Lib\\Win32\\"+Path;
	LibDirs+=L";"+RootDir+L"\\Lib\\Win32\\"+Path+"\\psdk";
   }
 else if(IDEVersion>=15&&Platform==L"64")
   {
	LibDirs=RootDir+L"\\Lib\\Win64\\"+Path;
	LibDirs+=L";"+RootDir+L"\\Lib\\Win64\\"+Path+"\\psdk";
   }
 else if(IDEVersion>=19&&Platform==L"AD")
   {
	LibDirs=RootDir+L"\\Lib\\Android\\"+Path;
	LibDirs+=L";"+RootDir+L"\\Lib\\Android\\"+Path+"\\armeabi";
   }
 else if(IDEVersion>=23&&Platform==L"A6")
   {
	LibDirs=RootDir+L"\\Lib\\Android64\\"+Path;
	LibDirs+=L";"+RootDir+L"\\Lib\\Android64\\"+Path+"\\armeabi";
   }
 else if(IDEVersion>=19&&IDEVersion<22&&Platform==L"IO")
   {
	LibDirs=RootDir+L"\\Lib\\iosDevice\\"+Path;
   }
 else if(IDEVersion>=22&&Platform==L"IO")
   {
	LibDirs=RootDir+L"\\Lib\\iosDevice32\\"+Path;
   }
 else if(IDEVersion>=22&&Platform==L"I6")
   {
	LibDirs=RootDir+L"\\Lib\\iosDevice64\\"+Path;
   }
 else if(IDEVersion>=17&&Platform==L"OS")
   {
	LibDirs=RootDir+L"\\Lib\\osx32\\"+Path;
   }
 else if(IDEVersion>=23&&Platform==L"O6")
   {
	LibDirs=RootDir+L"\\Lib\\osx64\\"+Path;
   }
 else
   {
	LibDirs=RootDir+L"\\Lib";
	LibDirs+=L";"+RootDir+L"\\Lib\\Obj";
	LibDirs+=L";"+RootDir+L"\\Lib\\"+Path;
	LibDirs+=L";"+RootDir+L"\\Lib\\psdk";
	if(IsRelease)
	  LibDirs+=L";"+RootDir+L"\\Lib\\Indy10";
	else
	  LibDirs+=L";"+RootDir+L"\\Lib\\"+Path+"\\Indy10";
   }
 return LibDirs;
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TForm_Main::GetSearchPaths(TQJson *QJson)
{
 UnicodeString SearchPaths,RootDir,Platform,BoostDir;
 int IDEVersion;
 RootDir=QJson->ForcePath("RootDir")->AsString;
 SearchPaths=QJson->ForcePath("BrowsingPath")->AsString;
 if(SearchPaths!=L"")
   {
	SearchPaths=Repl(SearchPaths,L"$(BDS)",RootDir);
	SearchPaths=Repl(SearchPaths,L"$(BDSINCLUDE)",RootDir+L"\\Include");
   }
 return SearchPaths;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::CompilerJob(PQJob AJob)
{
/* TODO : CompilerJob */
 TQJson *QJson=(TQJson *)AJob->Data;
 UnicodeString RootDir,Compiler,CompilerOptions,Platform,PlatformName,CmdStr,FileName,Result,FullResult,ExtName;
 UnicodeString LibDirs,IncludeDirs;
 UnicodeString OutFinalDir,OutIncludeDir,OutLibDir;
 UnicodeString ExtraAlias,ExtraNameSpaceSearch,ExtraIncludeDir,ExtraLibDir,ExtraSearchDir;
 int IDEVersion,MyRTLVersion,MyProductVersion;
 bool IsShowCMD=QJson->ForcePath("Project.ShowCMD")->AsBoolean;
 bool IsShowAllLog=QJson->ForcePath("Project.ShowAllLog")->AsBoolean;
 bool IsDebug=QJson->ForcePath("Project.BuildDebug")->AsBoolean;
 bool IsNameSpace=QJson->ForcePath("Project.NameSpaces")->AsBoolean;
 bool IsBuildAllUnits=QJson->ForcePath("Project.BuildAllUnits")->AsBoolean;
 bool IsAddIDEVersion=QJson->ForcePath("Project.AddIDEVersion")->AsBoolean;
 RootDir=QJson->ForcePath("RootDir")->AsString;
 IDEVersion=QJson->ForcePath("IDEVersion")->AsInteger;
 MyRTLVersion=QJson->ForcePath("RTLVersion")->AsInteger;
 MyProductVersion=QJson->ForcePath("ProductVersion")->AsInteger;
 FileName=QJson->ForcePath("Project.FileName")->AsString;
 ExtName=ExtractFileExt(FileName);
 PlatformName=QJson->ForcePath("Project.PlatformName")->AsString;
 Platform=QJson->ForcePath("Project.Platform")->AsString;
 OutFinalDir=QJson->ForcePath("Project.OutFinalDir")->AsString;
 if(!IsDebug)
   OutFinalDir+=L"\\Release";
 else
   OutFinalDir+=L"\\Debug";
 OutFinalDir+=L"\\"+QJson->ForcePath("Ver")->AsString;
 if(Platform==L"64")
   OutFinalDir+=L"\\Win64";
 else if(Platform==L"AD")
   OutFinalDir+=L"\\Android";
 else if(Platform==L"A6")
   OutFinalDir+=L"\\Android64";
 else if(Platform==L"IO")
   OutFinalDir+=L"\\IOS";
 else if(Platform==L"I6")
   OutFinalDir+=L"\\IOS64";
 else if(Platform==L"OS")
   OutFinalDir+=L"\\OSX";
 else if(Platform==L"O6")
   OutFinalDir+=L"\\OSX64";
 OutIncludeDir=QJson->ForcePath("Project.OutIncludeDir")->AsString;
 if(!IsDebug)
   OutIncludeDir+=L"\\Release";
 else
   OutIncludeDir+=L"\\Debug";
 OutIncludeDir+=L"\\"+QJson->ForcePath("Ver")->AsString;
 if(Platform==L"64")
   OutIncludeDir+=L"\\Win64";
 else if(Platform==L"AD")
   OutIncludeDir+=L"\\Android";
 else if(Platform==L"A6")
   OutIncludeDir+=L"\\Android64";
 else if(Platform==L"IO")
   OutIncludeDir+=L"\\IOS";
 else if(Platform==L"I6")
   OutIncludeDir+=L"\\IOS64";
 else if(Platform==L"OS")
   OutIncludeDir+=L"\\OSX";
 else if(Platform==L"O6")
   OutIncludeDir+=L"\\OSX64";
 OutLibDir=QJson->ForcePath("Project.OutLibDir")->AsString;
 if(!IsDebug)
   OutLibDir+=L"\\Release";
 else
   OutLibDir+=L"\\Debug";
 OutLibDir+=L"\\"+QJson->ForcePath("Ver")->AsString;
 if(Platform==L"64")
   OutLibDir+=L"\\Win64";
 else if(Platform==L"AD")
   OutLibDir+=L"\\Android";
 else if(Platform==L"A6")
   OutLibDir+=L"\\Android64";
 else if(Platform==L"IO")
   OutLibDir+=L"\\IOS";
 else if(Platform==L"I6")
   OutLibDir+=L"\\IOS64";
 else if(Platform==L"OS")
   OutLibDir+=L"\\OSX";
 else if(Platform==L"O6")
   OutLibDir+=L"\\OSX64";
 ExtraAlias=QJson->ForcePath("Project.ExtraAlias")->AsString;
 ExtraNameSpaceSearch=QJson->ForcePath("Project.ExtraNameSpaceSearch")->AsString;
 ExtraIncludeDir=QJson->ForcePath("Project.ExtraIncludeDir")->AsString;
 ExtraLibDir=QJson->ForcePath("Project.ExtraLibDir")->AsString;
 ExtraSearchDir=QJson->ForcePath("Project.ExtraSearchDir")->AsString;
 LibDirs=OutLibDir+L";"+ExtraLibDir+L";"+GetLibPaths(QJson)+L";"+ExtraSearchDir/*+L";"+GetSearchPaths(QJson)*/;
 LibDirs=GetQuotePaths(LibDirs,true);
 IncludeDirs=OutIncludeDir+L";"+ExtraIncludeDir+L";"+GetIncludePaths(QJson);
 IncludeDirs=GetQuotePaths(IncludeDirs,true);
 if(!DirectoryExists(OutFinalDir))
   {
	ForceDirectories(OutFinalDir);
	if(!DirectoryExists(OutFinalDir))
	  {
	   Result=_(L"不能创建目录")+L"("+PlatformName+L"):"+OutFinalDir;
	   SetCompilerResult(Result);
	   return;
	  }
   }
 if(!DirectoryExists(OutIncludeDir))
   {
	ForceDirectories(OutIncludeDir);
	if(!DirectoryExists(OutIncludeDir))
	  {
	   Result=_(L"不能创建目录")+L"("+PlatformName+L"):"+OutIncludeDir;
	   SetCompilerResult(Result);
	   return;
	  }
   }
 if(!DirectoryExists(OutLibDir))
   {
	ForceDirectories(OutLibDir);
	if(!DirectoryExists(OutLibDir))
	  {
	   Result=_(L"不能创建目录")+L"("+PlatformName+L"):"+OutLibDir;
	   SetCompilerResult(Result);
	   return;
	  }
   }
 OutFinalDir=GetQuotePaths(OutFinalDir,true);
 OutIncludeDir=GetQuotePaths(OutIncludeDir,true);
 OutLibDir=GetQuotePaths(OutLibDir,true);
 int p=ExtList->IndexOf(ExtName);
 CmdStr=L"";
 if(p<0||p>7)
   {
	Result=_(L"不支持此文件类型")+L"("+PlatformName+"):("+FileName+")!";
	SetCompilerResult(Result);
	return;
   }
 else if(p<3)
   {
	if(Platform==L"32"||Platform==L"64")
	  Compiler=L"\""+RootDir+L"\\Bin\\dcc"+Platform+L".exe\"";
	else if(Platform==L"AD")
	  Compiler=L"\""+RootDir+L"\\Bin\\dccaarm.exe\"";
	else if(Platform==L"A6")
	  Compiler=L"\""+RootDir+L"\\Bin\\dccaarm64.exe\"";
	else if(Platform==L"IO")
	  Compiler=L"\""+RootDir+L"\\Bin\\dcciosarm.exe\"";
	else if(Platform==L"I6")
	  Compiler=L"\""+RootDir+L"\\Bin\\dcciosarm64.exe\"";
	else if(Platform==L"OS")
	  Compiler=L"\""+RootDir+L"\\Bin\\dccosx.exe\"";
	else if(Platform==L"O6")
	  Compiler=L"\""+RootDir+L"\\Bin\\dccosx64.exe\"";
	if(!IsDebug)
	  CompilerOptions=L" -DNDEBUG";
	else
	  CompilerOptions=L" -DDEBUG;_DEBUG -V -VN";
	if(p==0&&IsNameSpace)
	  CompilerOptions+=L" -JPHNE";
	else if(p==0)
	  CompilerOptions+=L" -JPHE";
	else
	  CompilerOptions+=L" -JL";
	if(IDEVersion>15)
	  CompilerOptions+=L" -AGenerics.Collections=System.Generics.Collections;Generics.Defaults=System.Generics.Defaults;WinTypes=Winapi.Windows;WinProcs=Winapi.Windows;DbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE";
	else
	  CompilerOptions+=L" -ADbiTypes=BDE;DbiProcs=BDE;DbiErrs=BDE";
	if(ExtraAlias!=L"")
	  {
	   CompilerOptions+=L";"+ExtraAlias;
	  }
	CompilerOptions+=L" -NSWinapi;System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;System;Xml;Data;Datasnap;Web;Soap;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell";
	if(ExtraNameSpaceSearch!=L"")
	  {
	   CompilerOptions+=L";"+ExtraNameSpaceSearch;
	  }
	CompilerOptions+=L" -I"+LibDirs+L" -O"+LibDirs+L" -R"+LibDirs+L" -U"+LibDirs;
	if(IDEVersion<17)
	  CompilerOptions+=L" -E"+OutFinalDir+L" -LE"+OutFinalDir+L" -LN"+OutLibDir+L" -N0"+OutLibDir+L" -NB"+OutLibDir+L" -NO"+OutLibDir+L" -NH"+OutIncludeDir;
	else
	  CompilerOptions+=L" -E"+OutFinalDir+L" -LE"+OutFinalDir+L" -LN"+OutLibDir+L" -NU"+OutLibDir+L" -NB"+OutLibDir+L" -NO"+OutLibDir+L" -NH"+OutIncludeDir;
	CompilerOptions+=L" --BCB";
	if(IsBuildAllUnits)
	  CompilerOptions+=L" -B";
   }
 else if(p==3||p==7)
   {
	if(DotNETPath==L"")
	  {
	   Result=_(L"没有安装.NetFramework，所以不支持此文件类型")+L"("+PlatformName+L"):("+FileName+L")!";
	   SetCompilerResult(Result);
	   return;
	  }
	CmdStr=ComSpec+L" /c ";
	CmdStr+=L"SET BDS="+RootDir+L"&";
	CmdStr+=L"SET BDSINCLUDE="+RootDir+L"\\Include&";
	if(Platform==L"32"&&QJson->ForcePath("BoostDir")->AsString!=L"")
	  CmdStr+=L"SET CG_BOOST_ROOT="+QJson->ForcePath("BoostDir")->AsString+L"&";
	else if(Platform==L"64"&&QJson->ForcePath("Boost64Dir")->AsString!=L"")
	  CmdStr+=L"SET CG_64_BOOST_ROOT="+QJson->ForcePath("Boost64Dir")->AsString+L"&";
	Compiler=L"\""+DotNETPath+L"\\MSBuild.exe\"";
	if(!IsDebug)
	  CompilerOptions=L" /t:Rebuild /p:Config=Release;";
	else
	  CompilerOptions=L" /t:Rebuild /p:Config=Debug;";
	if(Platform==L"32"||Platform==L"64")
	  CompilerOptions+=L"Platform=Win"+Platform;
	else if(Platform==L"AD")
	  CompilerOptions+=L"Platform=Android";
	else if(Platform==L"A6")
	  CompilerOptions+=L"Platform=Android64";
	else if(Platform==L"IO"&&IDEVersion<22)
	  CompilerOptions+=L"Platform=iOSDevice";
	else if(Platform==L"IO")
	  CompilerOptions+=L"Platform=iOSDevice32";
	else if(Platform==L"I6")
	  CompilerOptions+=L"Platform=iOSDevice64";
	else if(Platform==L"OS")
	  CompilerOptions+=L"Platform=OSX32";
	else if(Platform==L"O6")
	  CompilerOptions+=L"Platform=OSX64";
	if(p==3)
	  {
	   CompilerOptions+=L";DCC_ExeOutput="+OutFinalDir;
	   CompilerOptions+=L";DCC_BplOutput="+OutFinalDir;
	   CompilerOptions+=L";DCC_HppOutput="+OutIncludeDir;
	   CompilerOptions+=L";DCC_ObjOutput="+OutLibDir;
	   CompilerOptions+=L";DCC_BpiOutput="+OutLibDir;
	   CompilerOptions+=L";DCC_DcuOutput="+OutLibDir;
	   CompilerOptions+=L";DCC_DcpOutput="+OutLibDir;
	   CompilerOptions+=L";DCC_UnitSearchPath=\""+GetQuotePaths(LibDirs)+L"\"";
	   CompilerOptions+=L";DCC_CBuilderOutput=All";
	  }
	else if(p==7)
	  {
//	   if(FileExists(ChangeFileExt(FileName,".res")))
//		 DeleteFile(ChangeFileExt(FileName,".res"));
	   CompilerOptions+=L";IncludePath=\""+GetQuotePaths(IncludeDirs)+L"\"";
	   CompilerOptions+=L";ILINK_LibraryPath=\""+GetQuotePaths(LibDirs)+L"\"";
	   CompilerOptions+=L";FinalOutputDir="+OutFinalDir;
	   CompilerOptions+=L";IntermediateOutputDir="+OutLibDir;
	   CompilerOptions+=L";BPILibOutputDir="+OutLibDir;
	  }
   }
 else if(p==5||p==6)
   {
	if(!FileExists(RootDir+L"\\Bin\\Bpr2Mak.exe"))
	  {
	   Result=_(L"缺少Bpr2Mak.exe，所以不支持此文件类型")+L"("+PlatformName+L"):("+FileName+L")!";
	   SetCompilerResult(Result);
	   return;
	  }
	if(!FileExists(RootDir+L"\\Bin\\Make.exe"))
	  {
	   Result=_(L"缺少Make.exe，所以不支持此文件类型")+L"("+PlatformName+L"):("+FileName+L")!";
	   SetCompilerResult(Result);
	   return;
	  }
	CmdStr=ComSpec+L" /c ";
	TCHAR ShortFileName[MAX_PATH];
	GetShortPathName(FileName.c_str(),ShortFileName,MAX_PATH);
	FileName=ShortFileName;
	Result=RootDir+L"\\Bin\\bpr2mak.exe";
	GetShortPathName(Result.c_str(),ShortFileName,MAX_PATH);
	Result=ShortFileName;
	CmdStr+=Result+L" -o"+ChangeFileExt(FileName,L".mak")+L" "+FileName+L"&";
	Result=RootDir+L"\\Bin\\make.exe";
	GetShortPathName(Result.c_str(),ShortFileName,MAX_PATH);
	Result=ShortFileName;
	CmdStr+=Result+L" -f"+ChangeFileExt(FileName,L".mak");
   }
 else if(p==4)
   {
	if(Platform==L"32"||Platform==L"64")
	  Compiler=L"\""+RootDir+L"\\Bin\\bcc"+Platform+L".exe\"";
	else if(Platform==L"AD")
	  Compiler=L"\""+RootDir+L"\\Bin\\bccaarm.exe\"";
	else if(Platform==L"A6")
	  Compiler=L"\""+RootDir+L"\\Bin\\bccaarm64.exe\"";
	else if(Platform==L"IO")
	  Compiler=L"\""+RootDir+L"\\Bin\\bcciosarm.exe\"";
	else if(Platform==L"I6")
	  Compiler=L"\""+RootDir+L"\\Bin\\bcciosarm64.exe\"";
	else if(Platform==L"OS")
	  Compiler=L"\""+RootDir+L"\\Bin\\bccosx.exe\"";
	else if(Platform==L"O6")
	  Compiler=L"\""+RootDir+L"\\Bin\\bccosx64.exe\"";
	if(!IsDebug)
	  CompilerOptions=L" -DNDEBUG";
	else
	  CompilerOptions=L" -DDEBUG;_DEBUG -v";
	if(Platform==L"64")
	  CompilerOptions=" -I"+IncludeDirs+" -L "+LibDirs+L" -n "+OutLibDir+L" -c -O2";
	else
	  {
	   CompilerOptions=" -I"+IncludeDirs+" -L"+LibDirs+L" -n"+OutLibDir+L" -Q -c";
	   if(IDEVersion>=14)
		 CompilerOptions+=L" -tU -C8 ";
	   CompilerOptions+=L" -w-par -O2 ";
      }
   }
 if(p!=5&&p!=6)
   CmdStr+=Compiler+L" "+CompilerOptions+L" \""+FileName+L"\"";
 SetCompilerResult(_(L"开始编译")+L"("+PlatformName+L" RTLVersion="+IntToStr(MyRTLVersion)+L" IDEVersion="+IntToStr(IDEVersion)+L" ProductVersion="+IntToStr(MyProductVersion)+L"):"+FileName);
 SetCompilerResult(L"============================================================");
 if(IsAddIDEVersion)
   {
	ExtName=FileName;
	CheckDPKSuffix(ExtName,IDEVersion);
	if(ExtName!=FileName)
	  {
	   SetCompilerResult(_(L"添加IDE版本号失败，错误描述：")+ExtName);
	   SetCompilerResult(L"============================================================");
	  }
   }
 if(IsShowCMD)
   {
	SetCompilerResult(CmdStr);
	SetCompilerResult(L"============================================================");
   }
 Result=PipeCall(CmdStr,ExcludeTrailingPathDelimiter(ExtractFilePath(FileName)));
 FullResult=Result;
 CheckResult(Result,QJson->ForcePath("Project.GridFileName")->AsString,QJson->ForcePath("Ver")->AsString+L"_"+Platform);
 if(!IsShowAllLog)
   Result=FilterResult(Result);
 SetCompilerResult(Result);
 FullLogTxt+=_(L"完整命令行：")+L"\r\n";
 FullLogTxt+=CmdStr+L"\r\n";
 FullLogTxt+=_(L"完整日志：")+L"\r\n";
 FullLogTxt+=FullResult+L"\r\n";
 SetCompilerResult(L"============================================================");
 Sleep(100);
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TForm_Main::FilterResult(UnicodeString Result)
{
/* TODO : FilterResult */
 TStringList *List=new TStringList();
 List->Text=Result;
 UnicodeString Str;
 bool IsOK=true;
 for(int i=0;i<List->Count;)
   {
	Str=List->Strings[i].LowerCase().Trim();
	if(Str==L"")
	  List->Delete(i);
	else if(Str.Pos(L"hint: ")>0||Str.Pos(L"hint: h")>0||Str.Pos(L"hint h")>0||Str.Pos(L"): hint ")>0)
	  {
	   i++;
	  }
	else if(Str.Pos(L"warning: ")>0||Str.Pos(L"warning: w")>0||Str.Pos(L"warning w")>0||Str.Pos(L"): warning ")>0)
	  {
	   i++;
	  }
	else if(Str.Pos(L"error: ")>0||Str.Pos(L"error: e")>0||Str.Pos(L"error e")>0||Str.Pos(L"): error ")>0)
	  {
	   i++;
	   IsOK=false;
	  }
	else if(Str.Pos(L"fatal: ")>0||Str.Pos(L"fatal: f")>0||Str.Pos(L"fatal f")>0||Result.Pos(L"): fatal ")>0)
	  {
	   i++;
	   IsOK=false;
	  }
	else
	  {
	   List->Delete(i);
	  }
   }
 if(IsOK)
   {
	List->Add(_(L"编译成功！"));
   }
 else
   {
	List->Add(_(L"编译失败！"));
   }
 Result=List->Text;
 delete List;
 return Result;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::RegisterFileType(UnicodeString ExtName,UnicodeString AppName)
{
 TRegistry *Registry = new TRegistry();
 try
   {
	try
	  {
	   Registry->Access=KEY_ALL_ACCESS;
	   Registry->RootKey=HKEY_CLASSES_ROOT;
	   UnicodeString ExtName2;
	   ExtName=ExtName.LowerCase();
	   if(ExtName.SubString(1,1)==L".")
		 {
		  ExtName2=ExtName;
		  ExtName=ExtName.SubString(2,ExtName.Length()-1);
		 }
	   else
		 {
		  ExtName2="."+ExtName;
		 }
	   Registry->OpenKey(ExtName2,true);
	   Registry->WriteString(L"",ExtName+L"file");
	   Registry->CloseKey();
	   Registry->OpenKey(ExtName+L"file",true);
	   Registry->OpenKey(L"DefaultIcon",true);
	   Registry->WriteString(L"",AppName+L",0");
	   Registry->CloseKey();
	   Registry->OpenKey(ExtName+L"file\\shell\\open\\command",true);
	   Registry->WriteString(L"",AppName+L" \"%1\"");
	   Registry->CloseKey();
	   SHChangeNotify(SHCNE_ASSOCCHANGED,SHCNF_IDLIST,NULL,NULL);
	  }
	catch(Exception &e)
	  {
	   Application->MessageBox(_(L"需要有管理员权限才能注册上CompilerCfg文件！\r\n请以管理员身份运行些程序！").c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	  }
   }
 __finally
   {
	delete Registry;
	Registry=NULL;
   }
}
//---------------------------------------------------------------------------
bool __fastcall TForm_Main::FileTypeIsRegister(UnicodeString ExtName)
{
 bool r=false;
 TRegistry *Registry = new TRegistry();
 try
   {
	try
	  {
	   Registry->Access=KEY_READ;
	   Registry->RootKey=HKEY_CLASSES_ROOT;
	   UnicodeString ExtName2;
	   ExtName=ExtName.LowerCase();
	   if(ExtName.SubString(1,1)==L".")
		 {
		  ExtName2=ExtName;
		  ExtName=ExtName.SubString(2,ExtName.Length()-1);
		 }
	   else
		 {
		  ExtName2="."+ExtName;
		 }
	   r=Registry->KeyExists(ExtName2);
	  }
	catch(Exception &e)
	  {
	   UnicodeString Str=_(L"检测类型注册情况出错，错误描述：")+e.Message;
	   Application->MessageBox(Str.c_str(),_(L"错误").c_str(),MB_OK+MB_ICONSTOP+MB_TOPMOST);
	  }
   }
 __finally
   {
	delete Registry;
	Registry=NULL;
   }
 return r;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnClearLogClick(TObject *Sender)
{
 Log->Lines->Clear();
 FullLogTxt=L"";
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnSaveLogClick(TObject *Sender)
{
 SD->DefaultExt=L"Log";
 SD->Filter=_(L"日志文件")+L"|*.Log";
 SD->Title=_(L"选择保存配置文件");
 UnicodeString Temp=L"";
 if(CfgName!=L"")
   {
	Temp=ChangeFileExt(CfgName,L".Compiler.Log");
	SD->InitialDir=ExtractFilePath(CfgName);
	SD->FileName=Temp;
   }
 if(SD->Execute())
   {
	Temp=SD->FileName;
	Log->Lines->SaveToFile(Temp);
   }
}
//---------------------------------------------------------------------------
UnicodeString __fastcall TForm_Main::ReplPlatformPath(UnicodeString Path,TQJson *QJson)
{
 Path=StringReplace(Path,L"%BDS%",QJson->ForcePath("RootDir")->AsString,TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 Path=StringReplace(Path,L"%Ver%",QJson->ForcePath("Ver")->AsString,TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 if(QJson->ForcePath("Project.Platform")->AsString==L"32")
   {
	UnicodeString Str=StringReplace(Path,L"%Platform%",L"",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
	Path=Str+L";"+Path;
	Path=StringReplace(Path,L"%Platform%",L"Win32",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
   }
 else if(QJson->ForcePath("Project.Platform")->AsString==L"64")
   Path=StringReplace(Path,L"%Platform%",L"Win64",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 else if(QJson->ForcePath("Project.Platform")->AsString==L"AD")
   Path=StringReplace(Path,L"%Platform%",L"Android",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 else if(QJson->ForcePath("Project.Platform")->AsString==L"A6")
   Path=StringReplace(Path,L"%Platform%",L"Android64",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 else if(QJson->ForcePath("Project.Platform")->AsString==L"IO")
   Path=StringReplace(Path,L"%Platform%",L"IOS",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 else if(QJson->ForcePath("Project.Platform")->AsString==L"I6")
   Path=StringReplace(Path,L"%Platform%",L"IOS64",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 else if(QJson->ForcePath("Project.Platform")->AsString==L"OS")
   Path=StringReplace(Path,"%Platform%",L"OSX",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 else if(QJson->ForcePath("Project.Platform")->AsString==L"O6")
   Path=StringReplace(Path,"%Platform%",L"OSX64",TReplaceFlags() << rfReplaceAll << rfIgnoreCase);
 return Path;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnDownClick(TObject *Sender)
{
 if(NG->SelectedCount>0)
   {
	if(NG->SelectedRow<NG->RowCount-1)
	  {
	   NG->MoveRow(NG->SelectedRow,NG->SelectedRow+1);
	   NG->SelectedRow++;
	  }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnUpClick(TObject *Sender)
{
 if(NG->SelectedCount>0)
   {
	if(NG->SelectedRow>0)
	  NG->MoveRow(NG->SelectedRow,NG->SelectedRow-1);
	  NG->SelectedRow--;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::CheckResult(UnicodeString Result,UnicodeString FileName,UnicodeString ColumnName)
{
/* TODO : CheckResult */
 bool IsOK=true;
 Result=Result.LowerCase();
 if(Result.Pos(L"error: ")>0||Result.Pos(L"error: e")>0||Result.Pos(L"error e")>0||Result.Pos(L"): error ")>0)
   {
	IsOK=false;
   }
 else if(Result.Pos(L"fatal: ")>0||Result.Pos(L"fatal: f")>0||Result.Pos(L"fatal f")>0||Result.Pos(L"): fatal ")>0)
   {
	IsOK=false;
   }
 TQJson *QJson=new TQJson();
 QJson->ForcePath("FileName")->AsString=FileName;
 QJson->ForcePath("ColumnName")->AsString=ColumnName;
 QJson->ForcePath("IsOK")->AsBoolean=IsOK;
 Workers->Post(ShowResultOnGrid,QJson,true,jdfFreeAsObject);
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::ShowResultOnGrid(PQJob AJob)
{
 UnicodeString Str,FileName,ColumnName;
 TQJson *QJson=(TQJson *)AJob->Data;
 bool IsOK;
 FileName=QJson->ForcePath("FileName")->AsString;
 ColumnName=QJson->ForcePath("ColumnName")->AsString;
 IsOK=QJson->ForcePath("IsOK")->AsBoolean;
 for(int i=0;i<NG->RowCount;i++)
   {
	Str=NG->CellByName["NGFile"][i]->AsString;
	if(Str==FileName)
	  {
	   if(IsOK)
		 NG->CellByName[ColumnName][i]->Color=clLime;
	   else
		 NG->CellByName[ColumnName][i]->Color=clRed;
	   break;
	  }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnRegTypeClick(TObject *Sender)
{
 RegisterFileType(L"CompilerCfg",Application->ExeName);
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::BtnOpenClick(TObject *Sender)
{
 OD->DefaultExt=L"CompilerCfg";
 OD->FileName=L"";
 OD->Filter=_(L"配置文件")+L"|*.CompilerCfg";
 OD->FilterIndex=0;
 OD->Title=_(L"请选择已经保存的配置文件");
 if(OD->Execute())
   {
	CfgName=OD->FileName;
	LoadProject(CfgName);
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::TabLogShow(TObject *Sender)
{
 BtnCompiler->Visible=false;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::TabProjectShow(TObject *Sender)
{
 BtnCompiler->Visible=true;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::TabReadmeShow(TObject *Sender)
{
 BtnCompiler->Visible=false;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::CheckDPKSuffix(UnicodeString &FileName,int IDEVersion)
{
 UnicodeString ExtName=ExtractFileExt(FileName).LowerCase();
 if(ExtList->IndexOf(ExtName)!=2)
   return;
 TStringList *List=new TStringList();
 try
   {
	try
	  {
	   List->LoadFromFile(FileName);
	   for(int i=0;i<List->Count;i++)
		 {
		  ExtName=List->Strings[i].LowerCase();
		  if(ExtName.Pos(L"{$libsuffix ")==1)
			{
			 List->Strings[i]="{$LIBSUFFIX '"+IntToStr(IDEVersion*10)+"'}";
			 List->SaveToFile(FileName);
			 break;
			}
		  else if(ExtName==L"{$implicitbuild off}")
			{
			 i=i-1;
			 List->Insert(i,"{$LIBSUFFIX '"+IntToStr(IDEVersion*10)+"'}");
			 List->SaveToFile(FileName);
			 break;
			}
		  else if(ExtName==L"requires")
			{
			 i=i-2;
			 List->Insert(i,"{$LIBSUFFIX '"+IntToStr(IDEVersion*10)+"'}");
			 List->SaveToFile(FileName);
			 break;
			}
		  else if(ExtName==L"contains")
			{
			 i=i-2;
			 List->Insert(i,"{$LIBSUFFIX '"+IntToStr(IDEVersion*10)+"'}");
			 List->SaveToFile(FileName);
			 break;
			}
		 }
	  }
	catch(Exception &e)
	  {
       FileName=e.Message;
	  }
   }
 __finally
   {
	delete List;
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GetCurrentSort(TQJson *QJson)
{
 UnicodeString FullFileName,FileName,ExtName;
 for(int i=0;i<NG->RowCount;i++)
   {
	  QJson->ForcePath("Files["+IntToStr(i)+L"].ID")->AsInteger=i;
	  QJson->ForcePath("Files["+IntToStr(i)+L"].NGFileName")->AsString=NG->CellByName["NGFile"][i]->AsString;
	  if(CfgName==L"")
	    FullFileName=NG->CellByName["NGFile"][i]->AsString;
	  else
	    FullFileName=GetCompleteFileName(ExtractFilePath(CfgName),NG->CellByName["NGFile"][i]->AsString);
	  QJson->ForcePath("Files["+IntToStr(i)+L"].FullFileName")->AsString=FullFileName;
	  FileName=ExtractFileName(FullFileName);
	  ExtName=ExtractFileExt(FullFileName);
	  QJson->ForcePath("Files["+IntToStr(i)+L"].FileName")->AsString=FileName;
	  QJson->ForcePath("Files["+IntToStr(i)+L"].ExtName")->AsString=ExtName;
	  QJson->ForcePath("Files["+IntToStr(i)+L"].Name")->AsString=FileName.SubString(1,FileName.Length()-ExtName.Length()).LowerCase();
    if(FileExists(FullFileName))
      {
       QJson->ForcePath("Files["+IntToStr(i)+L"].Need")->AsString=GetNeedFile(FullFileName).LowerCase();
      }
    else
      {
       QJson->ForcePath("Files["+IntToStr(i)+L"].Need")->AsString=L"";
       NG->CellByName["NGUse"][i]->AsBoolean=false;
      }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GetNewSort(TQJson *QJson)
{
 TStringList *ListS=new TStringList();
 TStringList *ListD=new TStringList();
 UnicodeString Name,Value,Tmp;
 int P,MaxP=0;
 for(int i=0;i<QJson->ForcePath("Files")->Count;i++)
   {
    ListS->Add(QJson->ForcePath("Files["+IntToStr(i)+L"].Name")->AsString+"="+QJson->ForcePath("Files["+IntToStr(i)+L"].Need")->AsString);
   }
// ListS->SaveToFile(ExtractFilePath(ParamStr(0))+"CurrentSort.Txt");
 for(;ListS->Count>0;)
   {
    Name=ListS->Names[0];
    Value=ListS->ValueFromIndex[0];
    ListS->Delete(0);
    for(int i=0;i<ListS->Count;i++)
      {
       Tmp=ListS->Names[i];
       if(Value.Pos(L"|"+Tmp+L"|")>0)
         {
          P=ListD->IndexOf(Tmp);
          if(P<0)
            {
             ListD->Add(Tmp);
             MaxP=ListD->IndexOf(Tmp);
            }
          else if(P>MaxP)
            {
             MaxP=P;
            }
         }
      }
    P=ListD->IndexOf(Name);
    if(P<0)
      {
       ListD->Add(Name);
      }
    else if(P<MaxP)
      {
       ListD->Move(P,MaxP);
      }
    MaxP=0;
   }
// ListD->SaveToFile(ExtractFilePath(ParamStr(0))+"NewSort.Txt");
 for(int i=0;i<QJson->ForcePath("Files")->Count;i++)
   {
    QJson->ForcePath("Files["+IntToStr(i)+L"].NewID")->AsInteger=ListD->IndexOf(QJson->ForcePath("Files["+IntToStr(i)+L"].Name")->AsString);
   }
 delete ListS;
 delete ListD;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::SortToNG(TQJson *QJson)
{
 int Old,New;
 UnicodeString FileName;
 for(int i=0;i<QJson->ForcePath("Files")->Count;i++)
   {
    FileName=QJson->ForcePath("Files["+IntToStr(i)+L"].NGFileName")->AsString;
    for(int j=0;j<NG->RowCount;j++)
      {
       if(FileName==NG->CellByName["NGFile"][j]->AsString)
         {
          Old=j;
          break;
         }
      }
    New=QJson->ForcePath("Files["+IntToStr(i)+L"].NewID")->AsInteger;
    if(Old!=New)
      {
       NG->MoveRow(Old,New);
      }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_CompilerSortClick(TObject *Sender)
{
 TQJson *QJson=new TQJson();
 GetCurrentSort(QJson);
 GetNewSort(QJson);
 SortToNG(QJson);
 NG->SelectFirstRow();
// QJson->SaveToFile(ExtractFilePath(ParamStr(0))+"CurrentSort.Json",teUTF8,true);
 delete QJson;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::PM_NoCompilerByRedClick(TObject *Sender)
{
 for(int i=0;i<NG->Columns->Count;i++)
	 {
	if(NG->Columns->Item[i]->ClassNameIs(L"TNxCheckBoxColumn")&&Name!=L"NGUse")
	    {
       for(int j=0;j<NG->RowCount;j++)
	       {
		      if(NG->CellByName["NGUse"][j]->AsBoolean)
			      {
			       if(NG->Cell[i][j]->AsBoolean)
				       {
					      if(NG->Cell[i][j]->Color==clRed)
                  {
                   NG->Cell[i][j]->AsBoolean=false;
                  }
               }
            }
         }
      }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::DelayRunWithCmdline(PQJob AJob)
{
 if(RunWithCmdline())
	 {
    if(FindCmdLineSwitch(L"Q",TSysCharSet()<<'\\'<<'/'<<'-',true))
		  Application->Terminate();
	 }
}
//---------------------------------------------------------------------------
bool __fastcall TForm_Main::RunWithCmdline()
{
 UnicodeString Temp,Param=L"",SID=L"",OutPath=L"",Ver=L"",Platform=L"",Alias=L"",ExInclude=L"",ExLib=L"",ExSearch=L"",SourcePath=L"";
 TStringList *Masks=new TStringList();
 bool IsDebug=false,LS=false,IsNameSpaces=true;
 bool r=true;
 int TmpInt;
 if(FindCmdLineSwitch(L"C",Param,true))
   {
    if(FileExists(Param)&&ExtractFileExt(Param).LowerCase()==L".compilercfg")
	    {
	     CfgName=Param;
	     LoadProject(CfgName);
      }
   }
 if(FindCmdLineSwitch(L"SID",Param,true))
   {
    SID=Param;
   }
 if(FindCmdLineSwitch(L"M",Param,true))
   {
	  if(Param!=L"")
	    {
	     while(Param.Length())
		     {
		      Temp=Param;
		      Param=DecodeTokenW(Temp,L";,",L'"',true,true,true);
		      if(Temp==L"")
			      {
			       Temp=Param;
			       Param=L"";
			      }
			   Masks->Add(Temp);
		    }
	    }
   }
 if(FindCmdLineSwitch(L"O",Param,true))
   {
	Param=GetCompleteFileName(CurrentDir,Param);
	OutPath=ExcludeTrailingPathDelimiter(Param)+"\\";
   }
 if(FindCmdLineSwitch(L"V",Param,true))
   {
	if(Param!=L"")
	  {
	   Ver=L"|";
	   while(Param.Length())
		 {
		  Temp=Param.UpperCase();
		  Param=DecodeTokenW(Temp,L";,",L'"',true,true,true);
		  if(Temp==L"")
			{
			 Temp=Param;
			 Param=L"";
			}
		  TmpInt=StrToIntDef(Temp,0);
		  if(Temp.SubString(1,1)==L"D"||Temp.SubString(1,1)==L"C"||Temp.SubString(1,2)==L"RS")
			Ver+=Temp+"|";
		  else if(Temp.SubString(1,2)==L"XE")
			Ver+=L"RS"+Temp+"|";
		  else if(TmpInt>=2000)
			Ver+=L"RS"+Temp+"|";
		  else if(TmpInt>6)
			Ver+=L"D"+Temp+"|";
		  else if(TmpInt>0)
			Ver+=L"D"+Temp+"|"+"C"+Temp+"|";
		 }
	  }
   }
 if(FindCmdLineSwitch(L"P",Param,true))
   {
	if(Param!=L"")
	  {
	   Platform=L"|";
	   while(Param.Length())
		 {
		  Temp=Param;
		  Param=DecodeTokenW(Temp,L";,",L'"',true,true,true);
		  if(Temp==L"")
			{
			 Temp=Param;
			 Param=L"";
			}
		  if(SameText(Temp,L"win32")||SameText(Temp,L"32")||SameText(Temp,L"w32"))
			Platform+=L"32|";
		  else if(SameText(Temp,L"win64")||SameText(Temp,L"64")||SameText(Temp,L"w64"))
			Platform+=L"64|";
		  else if(SameText(Temp,L"android")||SameText(Temp,L"ad")||SameText(Temp,L"android32")||SameText(Temp,L"an")||SameText(Temp,L"an32"))
			Platform+=L"AD|";
		  else if(SameText(Temp,L"android64")||SameText(Temp,L"a6")||SameText(Temp,L"an64")||SameText(Temp,L"ad64"))
			Platform+=L"A6|";
		  else if(SameText(Temp,L"ios")||SameText(Temp,L"io")||SameText(Temp,L"ios32"))
			Platform+=L"IO|";
		  else if(SameText(Temp,L"ios64")||SameText(Temp,L"i6"))
			Platform+=L"I6|";
		  else if(SameText(Temp,L"osx32")||SameText(Temp,L"os")||SameText(Temp,L"osx"))
			Platform+=L"OS|";
		  else if(SameText(Temp,L"osx64")||SameText(Temp,L"o6"))
			Platform+=L"O6|";
		 }
	  }
   }
 if(FindCmdLineSwitch(L"D",TSysCharSet()<<'\\'<<'/'<<'-',true))
   {
    IsDebug=true;
   }
 if(FindCmdLineSwitch(L"A",Param,true))
   {
    Alias=Param;
   }
 if(FindCmdLineSwitch(L"XI",Param,true))
   {
    ExInclude=Param;
   }
 if(FindCmdLineSwitch(L"XL",Param,true))
   {
    ExLib=Param;
   }
 if(FindCmdLineSwitch(L"XS",Param,true))
   {
    ExSearch=Param;
   }
 if(FindCmdLineSwitch(L"LS",TSysCharSet()<<'\\'<<'/'<<'-',true))
   {
    LS=true;
   }
 if(FindCmdLineSwitch(L"I",Param,true))
   {
	if(Param!=L"")
	  {
	   while(Param.Length())
		 {
		  SourcePath=Param;
		  Param=DecodeTokenW(SourcePath,L";,",L'"',true,true,true);
		  if(SourcePath==L"")
			{
			 SourcePath=Param;
			 Param=L"";
			}
		  SourcePath=GetCompleteFileName(CurrentDir,SourcePath);
		  if(FileOrDir(SourcePath)==2)
			{
			 SourcePath=ExcludeTrailingPathDelimiter(SourcePath)+L"\\";
       SourcePath=Repl(SourcePath,L"/",L"\\");
			 ScanFiles(SourcePath,Masks);
			}
		  else if(FileOrDir(SourcePath)==1)
			AddFile(SourcePath);
		 }
	  }
   }
 delete Masks;  
 if(FindCmdLineSwitch(L"NN",TSysCharSet()<<'\\'<<'/'<<'-',true))
   {
    IsNameSpaces=false;
   }
 ExtraNameSpaces->Checked=IsNameSpaces;
 BuildRelease->Checked=!IsDebug;
 BuildDebug->Checked=IsDebug;
 DpkAddIDEVersion->Checked=LS;
 if(ExtraAlias->Text!=L"")
   {
    ExtraAlias->Text=ExtraAlias->Text+L";";
   }
 ExtraAlias->Text=ExtraAlias->Text+Alias;
 if(ExtraIncludeDir->Text!=L"")
   {
    ExtraIncludeDir->Text=ExtraIncludeDir->Text+L";";
   }
 ExtraIncludeDir->Text=ExtraIncludeDir->Text+ExInclude;
 if(ExtraLibDir->Text!=L"")
   {
    ExtraLibDir->Text=ExtraLibDir->Text+L";";
   }
 ExtraLibDir->Text=ExtraLibDir->Text+ExLib;
 if(ExtraSearchDir->Text!=L"")
   {
    ExtraSearchDir->Text=ExtraSearchDir->Text+L";";
   }
 ExtraSearchDir->Text=ExtraSearchDir->Text+ExSearch;
 SelectVerAndPlatform(Ver,Platform);
 if(OutPath!=L"")
   {
    if(SID!=L"")
      {
       OutPath+=SID+L"\\";
      }
    OutFinalDir->Text=OutPath+L"Bin";
    OutIncludeDir->Text=OutPath+L"Include";
    OutLibDir->Text=OutPath+L"Lib";
   }
 if(NG->RowCount==0)
   r=false;
 if(OutFinalDir->Text==L"")
   r=false;
 if(OutIncludeDir->Text==L"")
   r=false;
 if(OutLibDir->Text==L"")
   r=false;
 if(r)
   {
    PM_CompilerSortClick(NULL);
    BtnCompiler->Click();
    if(OutPath!=L"")
      {
       int hFile = FileCreate(OutPath+L"Result.log");
       if(hFile>=0)
         {
          FileWrite(hFile,L"\xEF\xBB\xBF",3);
          FileWrite(hFile,FullLogTxt.c_str(),FullLogTxt.Length());
          FileClose(hFile);
         }     
       GenerateHTMLReport(OutPath);
       if(FindCmdLineSwitch(L"LR",TSysCharSet()<<'\\'<<'/'<<'-',true))
         {
          ShellExecute(Handle,NULL,(OutPath+L"Report.html").c_str(),NULL,NULL,SW_SHOWNORMAL);
         }
      }
   }
 return r;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::ScanFiles(UnicodeString Source,TStrings *Masks)
{
 TSearchRec sr;
 if(FindFirst(Source+L"*.*",faAnyFile,sr)==0)
   {
	  do
	    {
	     if((sr.Attr&faDirectory)&&sr.Name!=L"."&&sr.Name!=L"..")
		     {
		      ScanFiles(Source+sr.Name+L"\\",Masks);
		     }
	     else if(!(sr.Attr&faDirectory))
		     {
          if(Masks->Count==0)
			      AddFile(Source+sr.Name);
          else
            {
             for(int i=0;i<Masks->Count;i++)
               {
                if(StrLikeW(sr.Name.c_str(),(*Masks)[i].c_str(),true))
                  AddFile(Source+sr.Name);
               }
            }
         }
	    }while(FindNext(sr)==0);
	  FindClose(sr);
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::SelectVerAndPlatform(UnicodeString Ver,UnicodeString Platform)
{
 if(Ver==L""&&Platform==L"")
   return;
 UnicodeString ColumnName,CVer,CPlatform;
 int p;
 for(int i=0;i<NG->Columns->Count;i++)
   {
	  ColumnName=NG->Columns->Item[i]->Name;
	  p=ColumnName.Pos(L"_");
	  if(p>1)
	    {
	     CVer=ColumnName.SubString(1,p-1);
	     CPlatform=ColumnName.SubString(p+1,2);
       if(Ver==L""&&PosW(L"|"+CPlatform+L"|",Platform,true)==0)
         {
		      for(int j=0;j<NG->RowCount;j++)
			      NG->CellByName[ColumnName][j]->AsBoolean=false;
         }
       else if(Platform==L""&&PosW(L"|"+CVer+L"|",Ver,true)==0)
         {
		      for(int j=0;j<NG->RowCount;j++)
			      NG->CellByName[ColumnName][j]->AsBoolean=false;
         }
	     else if(Ver!=L""&&Platform!=L""&&(PosW(L"|"+CVer+L"|",Ver,true)==0||PosW(L"|"+CPlatform+L"|",Platform,true)==0))
		     {
		      for(int j=0;j<NG->RowCount;j++)
			      NG->CellByName[ColumnName][j]->AsBoolean=false;
         }
      }
   }
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::GenerateHTMLReport(UnicodeString Path)
{
	TQStringCatHelperW *ABuilder = new TQStringCatHelperW;
	String ATitle = _(L"编译报告")+L"-" + FormatDateTime(L"yyyy-mm-dd hh:nn:ss", Now());
	ABuilder->Cat(L"<!Doctype html><HTML>", -1);
	ABuilder->Cat(L"<head>\r\n", -1);
	ABuilder->Cat
		(L"<meta http-equiv=Content-Type content=\"text/html;charset=utf-8\">\r\n",
		-1);
	ABuilder->Cat(L"<title>", -1)->Cat(ATitle)->Cat(L"</title>", -1);
	// CSS
	ABuilder->Cat
		(L"<style type=\"text/css\">\r\n" "table.gridtable {\r\n"
		"font-family: verdana,arial,sans-serif;\r\n" "font-size:11px;\r\n"
		"color:#333333;\r\n" "border-width: 1px;\r\n"
		"border-color: #666666;\r\n" "border-collapse: collapse;\r\n" "}\r\n"
		"table.gridtable thead {\r\n" "border-width: 1px;\r\n"
		"padding: 8px;\r\n" "border-style: solid;\r\n"
		"border-color: #666666;\r\n" "background-color: #dedede;\r\n" "}\r\n"
		"table.gridtable td {\r\n" "border-width: 1px;\r\n" "padding: 8px;\r\n"
		"border-style: solid;\r\n" "border-color: #666666;\r\n"
		"background-color: #ffffff;\r\n" "}\r\n" "</style>\r\n", -1);
	ABuilder->Cat(L"</head>\r\n", -1);
	// 输出表头
	ABuilder->Cat(L"<center><body>\r\n<table class=\"gridtable\"><thead>", -1);
	ABuilder->Cat(L"<caption><h3>", -1)->Cat(ATitle)->Cat
		(L"</h3><br/><font color=\"#FF0000\">■&nbsp;&nbsp;")->Cat(_(L"红色-编译失败"))->Cat(L"</font>"
		L"&nbsp;&nbsp;<font color=\"#00FF00\">■&nbsp;&nbsp;")->Cat(_("绿色-编译通过"))->Cat(L"</font>"
		L"&nbsp;&nbsp;<font color=\"#000000\">□&nbsp;&nbsp;")->Cat(_(L"白色-平台未测试"))->Cat(L"</font>"
		L"</caption>",
		-1);
	ABuilder->Cat(L"<tr>", -1);
	for (int i = 0; i < NG->Columns->Count; i++)
		{
		ABuilder->Cat(L"<td>", -1)->Cat((UnicodeString)NG->Columns->Item[i]->Header->Caption)
			->Cat(L"</td>", -1);
		}
	ABuilder->Cat(L"</tr></thead>", -1);
	// 输出表内容
	ABuilder->Cat(L"<tbody>", -1);
	Nxcells::TCell *ACell;
	TNxCustomColumn *ACol;
	TNxCheckBoxColumn *ACheckCol;
	for (int row = 0; row < NG->RowCount; row++)
		{
		ABuilder->Cat(L"<tr>", -1);
		for (int col = 0; col < NG->Columns->Count; col++)
			{
			ACol = NG->Columns->Item[col];
			ACell = NG->Cell[col][row];
			ABuilder->Cat(L"<td ", -1);
			ACheckCol = dynamic_cast<TNxCheckBoxColumn*>(ACol);
			if (ACell->Color != clWindow)
				{
				if (ACheckCol && (!ACell->AsBoolean))
					{
					ABuilder->Cat(L"style=\"background-color:", -1)->Cat
						(ColorToWebColorStr(clGray))->Cat(L"\" ", -1);
					}
				else
					{
					ABuilder->Cat(L"style=\"background-color:", -1)->Cat
						(ColorToWebColorStr(ACell->Color))->Cat(L"\" ", -1);
					}
				}
			switch (ACol->Alignment)
				{
			case taLeftJustify:
				break;
			case taRightJustify:
				ABuilder->Cat(L"align=\"right\"", -1);
				break;
			case taCenter:
				ABuilder->Cat(L"align=\"center\"", -1);
				break;
				}
			ABuilder->Cat(L">", 1);
			ABuilder->Cat(L"<font color=\"", -1);
			ABuilder->Cat(ColorToWebColorStr(ACell->TextColor))->Cat
				(L"\">", -1);
			if (ACheckCol)
				{
				if (ACell->AsBoolean)
					{
					if (ACol->Index == 1)
						{
						ABuilder->Cat(L"√", -1);
						}
					}
				}
			else
				{
				ABuilder->Cat((UnicodeString)ACell->AsString);
				}
			ABuilder->Cat(L"</font>", -1);
			}
		ABuilder->Cat(L"</tr>\r\n", -1);
		}
	ABuilder->Cat(L"</tbody>", -1);
	ABuilder->Cat(L"</table>", -1);
	ABuilder->Cat(L"<p>")->Cat(_(L"日志"))->Cat(L"<p><textarea cols=120 rows=80> ", -1);
	ABuilder->Cat(HtmlEscape(Log->Text));
	ABuilder->Cat(L"</textarea></body></center>", -1);
	ABuilder->Cat(L"</HTML>", -1);
	SaveTextU(Path + "Report.html", ABuilder->Value, false);
	delete ABuilder;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::FormCloseQuery(TObject *Sender, bool &CanClose)
{
 if(BtnCompiler->Tag==1)
   {
	if(Application->MessageBox(_(L"正在编译中，确定要中止吗？").c_str(),_(L"关闭").c_str(),MB_YESNO+MB_ICONQUESTION+MB_TOPMOST)==IDNO)
      {
       CanClose=false;
      }
    else
      {
	     JobGroup->Cancel();
	     BtnCompiler->Tag=0;
	     P1->Enabled=true;
		 BtnCompiler->Caption=_(L"编译");
       CanClose=true;
      }
   }
 else
   CanClose=true;
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::SystemMenuCommand(TWMMenuSelect &Msg)
{
 if(Msg.IDItem==200)
   {
	UnicodeString Str;
	Str=Str.sprintf(_(L"命令行编译工具——%s版\n\n王氏工作室制作！").c_str(),MyGetFileVersion(Application->ExeName).c_str());
    Application->MessageBox(Str.c_str(),_(L"关于").c_str(),MB_OK+MB_ICONINFORMATION+MB_TOPMOST);
   }
 else if(Msg.IDItem==201)
   {
	OnlyFromConfig=!OnlyFromConfig;
	if(OnlyFromConfig)
	  CheckMenuItem(GetSystemMenu(Handle,false),201,MF_CHECKED);
	else
	  CheckMenuItem(GetSystemMenu(Handle,false),201,MF_UNCHECKED);
	CustomCfg->ForcePath("OnlyFromConfig")->AsBoolean=OnlyFromConfig;
	CustomCfg->SaveToFile(ExtractFilePath(Application->ExeName)+L"Data.Cfg",teUTF8,true);
   }
 else if(Msg.IDItem>=250&&Msg.IDItem<250+LangManager->Count)
   {
	int Index=Msg.IDItem-250;
	LangManager->ActiveIndex=Index;
	CustomCfg->ForcePath("LangID")->AsInteger=LangManager->ActiveLocale;
	CustomCfg->SaveToFile(ExtractFilePath(Application->ExeName)+L"Data.Cfg",teUTF8,true);
   }
 TForm::Dispatch(&Msg);
}
//---------------------------------------------------------------------------
void __fastcall TForm_Main::ChangeLangIDChange(TObject *Sender)
{
 LangManager->ActiveIndex=ChangeLangID->ItemIndex;
 CustomCfg->ForcePath("LangID")->AsInteger=LangManager->ActiveLocale;
 CustomCfg->SaveToFile(ExtractFilePath(Application->ExeName)+L"Data.Cfg",teUTF8,true);
}
//---------------------------------------------------------------------------

void __fastcall TForm_Main::DoLanguageChanged(TObject *ASender)
{
HMENU hSysMenu=GetSystemMenu(Handle,false);
int ACount=GetMenuItemCount(hSysMenu);
for (int i=0; i<ACount; i++)
	{
	if(GetMenuItemID(hSysMenu,i)==200)//About
		ModifyMenu(hSysMenu,i,MF_STRING,200,_(L"关于(&A)").c_str());
	else if (GetMenuItemID(hSysMenu,i)==201)
		ModifyMenu(hSysMenu,i,MF_STRING,200,_(L"只从配置文件读取").c_str());
	}
TNxCheckBoxColumn *ACol;
for(int i=0;i<NG->Columns->Count;i++)
   {
   ACol=(TNxCheckBoxColumn *)NG->Columns->Item[i];
   if (EndWithW(ACol->Name,L"_32",true))
	ACol->Header->Caption=CompilerList->Names[i]+_(L" 32位");
   else if (EndWithW(ACol->Name,L"_64",true))
	ACol->Header->Caption=CompilerList->Names[i]+_(L" 64位");
   else if(EndWithW(ACol->Name,L"_AD",true))
	ACol->Header->Caption=CompilerList->Names[i]+_(L" 安卓");
   else if(EndWithW(ACol->Name,L"_A6",true))
	ACol->Header->Caption=CompilerList->Names[i]+_(L" 安卓64");
	}
}
