** WÝNDOWS LOGON ÝÞLEMLERÝ **************************************************************************************
PROCEDURE wlogon
	PARAMETERS xuser,xdomain,xpassword
	*!*	BOOL LogonUser(
	*!*	  _In_      LPTSTR lpszUsername,
	*!*	  _In_opt_  LPTSTR lpszDomain,
	*!*	  _In_opt_  LPTSTR lpszPassword,
	*!*	  _In_      DWORD dwLogonType,
	*!*	  _In_      DWORD dwLogonProvider,
	*!*	  _Out_     PHANDLE phToken
	*!*	);


	#define LOGON32_PROVIDER_DEFAULT 0
	#define LOGON32_LOGON_INTERACTIVE       2
	#define LOGON32_LOGON_NETWORK           3
	#define LOGON32_LOGON_BATCH             4
	#define LOGON32_LOGON_SERVICE           5
	#define LOGON32_LOGON_UNLOCK            7
	DECLARE integer LogonUser IN AdvApi32.DLL;
	string szUsername,;
	string lpszDomain,;
	string lpszPassword,;
	integer dwLogonType,;
	integer dwLogonProvider,;
	integer @phToken
	DECLARE integer ImpersonateLoggedOnUser IN AdvApi32.DLL integer hToken
	DECLARE integer CloseHandle IN kernel32.DLL integer hToken
	local nToken
	nToken = 0
	=LogonUser(xuser,xdomain,xpassword,LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, @nToken)
	IF ntoken=0
	   RETURN .f.
	else   
	   =ImpersonateLoggedOnUser(nToken)
	   =closehandle(nToken)  && 28-01-2015
	   RETURN .t.
	endif   
	*
	*
ENDPROC


PROCEDURE wlogout
	DECLARE integer RevertToSelf IN AdvApi32.DLL
	= RevertToSelf()
	RETURN
ENDPROC

PROCEDURE TEST(i)
	DIME test(2)
	**array test[2]
	**LOCAL ARRAY test[2]
	test(1) = "bababa"
	test(2) = "AaaAaa"

	RETURN test(i)
ENDPROC
	
	