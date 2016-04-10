#ifndef DEBUG_HLP
#define DEBUG_HLP


///////////////////////////////////////////////////////////////
//Printing funcs and macros used for Debug//

#ifdef DBG

#define DEBUG_PRINT(str)  std::cout << str; cout.flush();
#define PRINT_OBJ(obj) DEBUG_PRINT( " obj{'"<< *((string*) (obj).data )<<"', @ "<<&obj<< "} "  )
#define DEBUG_LIST(list)/**/

#else

#define DEBUG_PRINT(str) //do { } while ( false )
#define DEBUG_LIST(list)
#define DEBUG_ALL

#endif

///////////// End debug functions/////////////////////
////////////////////////////////////////////////////
#endif //OS_EX3_ENVCONTROLLER_H
