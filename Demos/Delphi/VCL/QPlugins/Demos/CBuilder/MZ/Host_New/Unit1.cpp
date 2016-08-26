// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "Unit1.h"
#include "qplugins_loader_lib.hpp"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.dfm"
TForm_Host *Form_Host;

// ---------------------------------------------------------------------------
__fastcall TForm_Host::TForm_Host(TComponent* Owner) : TForm(Owner)
{
	String APath = ExtractFilePath(Application->ExeName);
	PluginsManager()->Loaders->Add(ToInterface<IQService>(new TQDLLLoader(APath,
		L".dll")));
	PluginsManager()->Start();
}

// ---------------------------------------------------------------------------
void __fastcall TForm_Host::Button1Click(TObject *Sender)
{
	_di_IQFormService AService;
	if (HasInterface(GetService(L"/Services/Docks/Forms/CB2010Static_Multi"),
		AService))
	{
		TTabSheet *APage = new TTabSheet(PageControl1);
		APage->PageControl = PageControl1;
		APage->Caption = ToInterface<IQService>(AService)->Name;
		AService->DockTo((UIntPtr)APage->Handle, faContent);
		HoldByComponent(this, AService);
	}
}

// ---------------------------------------------------------------------------
UnicodeString __fastcall EnumChildServices(_di_IQServices Root,
	UnicodeString Path = "")
{
	_di_IQService AService;
	_di_IQServices AChilds;
	UnicodeString List = "", Name;
	Path += L"/";
	for (int i = 0; i < Root->Count; i++)
	{
		AService = Root->Items[i];
		Name = AService->Name;
		if (HasInterface(AService, AChilds))
		{
			List += EnumChildServices(AChilds, Path + Name);
		}
		else
		{
			List += Path + Name + L"\n";
		}
	}
	return List;
}

// ---------------------------------------------------------------------------
void __fastcall TForm_Host::FormCreate(TObject *Sender)
{
	ShowMessage(EnumChildServices(PluginsManager()->Loaders, "Loaders") +
		EnumChildServices(PluginsManager()->Routers, "Routers") +
		EnumChildServices(PluginsManager()->Services, "Services"));
}

// ---------------------------------------------------------------------------
void __fastcall TForm_Host::Button2Click(TObject *Sender)
{
	if (HasInterface(PluginsManager()->ByPath
		(L"/Services/Docks/Forms/CB2010Static_Multi"), Form))
	{
		Form->Show();
	}
}

// ---------------------------------------------------------------------------
void __fastcall TForm_Host::Button3Click(TObject *Sender)
{
	_di_IQFormService AFormService;
	if (HasInterface(PluginsManager()->ByPath
		(L"/Services/Docks/Forms/CB2010Static_Multi"), AFormService))
	{
		AFormService->ShowModal(NULL, NULL);
	}
}

// ---------------------------------------------------------------------------
