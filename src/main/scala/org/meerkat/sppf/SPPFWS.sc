package org.meerkat.sppf

import scala.collection.mutable._


object SPPFWS {

	 val n: SPPFNode = NonterminalNode("A", 0, 5)
                                                  //> n  : org.meerkat.sppf.SPPFNode = A,0,5
	 
	 val l: Buffer[n.T] = n.children          //> l  : scala.collection.mutable.Buffer[org.meerkat.sppf.SPPFWS.n.T] = null
	 
	 l.map { x => x.children }                //> java.lang.NullPointerException
                                                  //| 	at org.meerkat.sppf.SPPFWS$$anonfun$main$1.apply$mcV$sp(org.meerkat.sppf
                                                  //| .SPPFWS.scala:12)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$$anonfun$$exe
                                                  //| cute$1.apply$mcV$sp(WorksheetSupport.scala:76)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.redirected(W
                                                  //| orksheetSupport.scala:65)
                                                  //| 	at org.scalaide.worksheet.runtime.library.WorksheetSupport$.$execute(Wor
                                                  //| ksheetSupport.scala:75)
                                                  //| 	at org.meerkat.sppf.SPPFWS$.main(org.meerkat.sppf.SPPFWS.scala:6)
                                                  //| 	at org.meerkat.sppf.SPPFWS.main(org.meerkat.sppf.SPPFWS.scala)

}