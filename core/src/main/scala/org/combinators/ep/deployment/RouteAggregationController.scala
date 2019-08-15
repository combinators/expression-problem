package org.combinators.ep.deployment

import javax.inject.Singleton
import play.api.mvc.{Handler, RequestHeader}
import play.api.routing.Router.{Routes => RRoutes}
import play.api.routing.SimpleRouter

/** Aggregates all routing entries of CodeGenerationControllers into a combined table. */
abstract class RouteAggregationController extends SimpleRouter {
  var controllers: Seq[CodeGenerationController[_]]

  override def routes: RRoutes =
    controllers.foldLeft(PartialFunction.empty[RequestHeader, Handler]){
      case (f, controller) => f.orElse(controller.routes)
    }

}
