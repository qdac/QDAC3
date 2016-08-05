//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
//---------------------------------------------------------------------------
#include <Vcl.Styles.hpp>
#include <Vcl.Themes.hpp>
USEFORM("Unit1.cpp", Form_Main);
USEFORM("Unit2.cpp", Form_PathSetup);
USEFORM("Unit3.cpp", Frame_Compiler); /* TFrame: File Type */
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int)
{
	try
	{
    if(FindCmdLineSwitch("?",TSysCharSet()<<'\\'<<'/'<<'-',true))
      {
       UnicodeString HelpStr=L"命令行编译工具命令行参数：\n";
	   HelpStr+=ExtractFileName(Application->ExeName)+L" [编译预配置文件]\n";
	   HelpStr+=L"可用参数：\n";
	   HelpStr+=L"/C 编译预配置文件,例: /C C:\\QDAC3.CompilerCfg\n";
	   HelpStr+=L"/NU 编译时不显示界面且自动退出程序\n";
	   HelpStr+=L"/Q 编译完成后退出程序\n";
	   HelpStr+=L"/LR 编译完成后显示编译报告\n";
	   HelpStr+=L"/I 源码路径列表,例: /I C:\\QDAC3\\Source;D:\\1.pas\n";
	   HelpStr+=L"/SID 会话编码,例: /SID swish\n";
	   HelpStr+=L"/M 源码文件掩码,例: /M *.pas;*21*.dpk\n";
	   HelpStr+=L"/O 目标路径(将自动建立Bin、Include、Lib目录),例: /O C:\\QDAC3\n";
	   HelpStr+=L"/V 测试版本列表,例: /V C6;D6;D7;RS2006;RS2007;RS2010;RSXE8\n";
	   HelpStr+=L"/P 目标平台列表,例: /P win32;win64;android;ios;ios64;osx;osx64\n";
	   HelpStr+=L"/D 编译为Debug版本(默认为Release版本)\n";
	   HelpStr+=L"/A 别名定义,例: /A WinTypes=Winapi.Windows\n";
	   HelpStr+=L"/XI 扩展的Include路径,例: /XI C:\\BCB\\Include\n";
	   HelpStr+=L"/XL 扩展的Lib搜索路径,例: /XL C:\\BCB\\Lib \n";
	   HelpStr+=L"/XS 扩展的搜索路径,例: /XS C:\\BCB\\Source\n";
	   HelpStr+=L"/NN 生成的Hpp中不包含命名空间\n";
	   HelpStr+=L"/LS 在DPK文件中添加IDE版本号\n\n";
	   HelpStr+=L"命令行和配置文件同时存在时，命令行参数会覆盖配置文件相应的参数！\n";
	   HelpStr+=L"参数I/M/V/P/A/XI/XL/XS中多个值中中间用;分隔，\n路径中有空格的，请在路径前后加\"\"\n";
       HelpStr+=L"例: /I \"C:\\QDAC3;D:\\AAA BBB;X:\\Dev\"";
       MessageBox(NULL,HelpStr.c_str(), L"帮助",MB_OK+MB_ICONINFORMATION+MB_TOPMOST);
      }
    else
	  {
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->Title = L"命令行编译工具";
		TStyleManager::TrySetStyle(L"Luna");
		Application->CreateForm(__classid(TForm_Main), &Form_Main);
		Application->Run();
      }
	}
	catch (Exception &exception)
	{
		Application->ShowException(&exception);
	}
	catch (...)
	{
		try
		{
			throw Exception("");
		}
		catch (Exception &exception)
		{
			Application->ShowException(&exception);
		}
	}
	return 0;
}
//---------------------------------------------------------------------------
