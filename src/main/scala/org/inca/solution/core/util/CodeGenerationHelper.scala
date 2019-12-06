package org.inca.solution.core.util

import org.inca.Node
import org.inca.core_old.content.IPatternBody
import org.inca.core_old.typeHint.{ITypeHintKeyProvider, UserObjectMap}

class CodeGenerationHelper {

//  public static void putUserObjectType(final node<ITypeHintKeyProvider> reference) {
//    final node<IPatternBody> body = reference.ancestor<concept = IPatternBody>;
//    final node<UserObjectMap> map = concept/UserObjectMap/.getOrCreateMap(body);
//    if (reference.getTypeHintKey() != null) {
//      final node<> oldType = map.get(reference.getTypeHintKey());
//      final node<> newType = reference.type;
//      if (oldType == null || (newType != null && isSubtype(newType :< oldType))) {
//        map.offer(reference.getTypeHintKey(), newType);
//      }
//    }
//  }

  def putUserObjectType(reference: Node[ITypeHintKeyProvider]): Unit = {
    val body: Node[IPatternBody] = reference.getAncestor[IPatternBody](concept = true)
    val map: Node[UserObjectMap] = ...
    if (reference.getType)
  }
}
