package com.quantel.qstack.scamp.fims.model

/** Details of a fault. This type can be extended by each service to provide additional error codes.
  */
case class Fault(
                  errorCode: ErrorCodeType,
                  detail: Option[String] = None)

object Http{
  val statusCodeDescriptions = Map(
    400 -> "Bad Request",
    403 -> "Forbidden",
    404 -> "Not Found",
    408 -> "Request timeout",
    409 -> "Conflict",
    412 -> "Precondition Failed",
    500 -> "Internal Server Error",
    502 -> "Bad Gateway",
    503 -> "Service unavailable.",
    504 -> "Gateway Timeout"
  )
}

abstract class ErrorCodeType(val description: String, val statusCode: Option[Int]){
  lazy val statusCodeDescription : Option[ String ] = {
    for (code <-statusCode; description <- Http.statusCodeDescriptions.get(code)) yield description
  }
}

object ErrorCodeType{
  def fromString(value: String): ErrorCodeType = {
    allCodes.find ( p => p.toString == value).getOrElse(throw new IllegalArgumentException("Error status code '" + value + "' does not match a known base status code."))
  }
  val allCodes: List[ErrorCodeType] = List(
    INF_S00_0001, INF_S00_0002, INF_S00_0003, INF_S00_0004,
    INF_S00_0005, INF_S00_0006, INF_S00_0007, SVC_S00_0001,
    SVC_S00_0002, SVC_S00_0003, SVC_S00_0004, SVC_S00_0005,
    SVC_S00_0006, SVC_S00_0007, SVC_S00_0008, SVC_S00_0009,
    SVC_S00_0010, SVC_S00_0011, SVC_S00_0012, SVC_S00_0013,
    SVC_S00_0014, SVC_S00_0015, SVC_S00_0016, SVC_S00_0017,
    SVC_S00_0018, SVC_S00_0019, SVC_S00_0020, SVC_S00_0021,
    SVC_S00_0022, DAT_S00_0001, DAT_S00_0002, DAT_S00_0003,
    DAT_S00_0004, DAT_S00_0005, DAT_S00_0006, DAT_S00_0007,
    DAT_S00_0008, DAT_S00_0009, DAT_S00_0010, DAT_S00_0011,
    DAT_S00_0012, DAT_S00_0013, DAT_S00_0014, EXT_S00_0000,
    SEC_S00_0001, SEC_S00_0002, SEC_S00_0003, SEC_S00_0004,
    SEC_S00_0005, SEC_S00_0006)

  case object INF_S00_0001 extends ErrorCodeType("System unavailable.", Some(503))
  case object INF_S00_0002 extends ErrorCodeType("System timeout.", Some(408))
  case object INF_S00_0003 extends ErrorCodeType("System internal error.", Some(500))
  case object INF_S00_0004 extends ErrorCodeType("Unable to connect to the database.",Some(500))
  case object INF_S00_0005 extends ErrorCodeType("System out of memory.", Some(500))
  case object INF_S00_0006 extends ErrorCodeType("System out of disk space.", Some(500))
  case object INF_S00_0007 extends ErrorCodeType("Maximum number of connections reached.",Some(503))
  case object SVC_S00_0001 extends ErrorCodeType("Job command is not currently supported by the service URI specified.", Some(403))
  case object SVC_S00_0002 extends ErrorCodeType("Queue command is not currently supported by the service or the device.",Some(403))
  case object SVC_S00_0003 extends ErrorCodeType("Operation requested is not currently supported by the service ot the device.",Some(403))
  case object SVC_S00_0004 extends ErrorCodeType("Service unable to find/lookup device endpoint.",Some(502))
  case object SVC_S00_0005 extends ErrorCodeType("Job command failed.",None)
  case object SVC_S00_0006 extends ErrorCodeType("Queue command failed.",None)
  case object SVC_S00_0007 extends ErrorCodeType("Service unable to connect to device endpoint.",Some(502))
  case object SVC_S00_0008 extends ErrorCodeType("Job queue is full, locked or stopped. No new jobs are being accepted.",Some(503))
  case object SVC_S00_0009 extends ErrorCodeType("Job ended with a failure.",None)
  case object SVC_S00_0010 extends ErrorCodeType("Service received no response from device.",Some(504))
  case object SVC_S00_0011 extends ErrorCodeType("Service received an exception from device. See description or exception detail.", Some(502))
  case object SVC_S00_0012 extends ErrorCodeType("Service received an unknown or an internal error from device. See description for error detail.", Some(502))
  case object SVC_S00_0013 extends ErrorCodeType("Unable to connect to client's notification service endpoint (replyTo) to send the asynchronous job result notification response.", None)
  case object SVC_S00_0014 extends ErrorCodeType("Unable to connect to client's service endpoint (faultTo) to send the asynchronous job fault response.", None)
  case object SVC_S00_0015 extends ErrorCodeType("Feature not supported.",Some(403))
  case object SVC_S00_0016 extends ErrorCodeType("Deadline passed.", Some(403))
  case object SVC_S00_0017 extends ErrorCodeType("Time constraints in request cannot be met.", Some(403))
  case object SVC_S00_0018 extends ErrorCodeType("Internal or unknown error encountered. See description for error detail.", Some(500))
  case object SVC_S00_0019 extends ErrorCodeType("Version mismatch.", Some(412))
  case object SVC_S00_0020 extends ErrorCodeType("License expired.", Some(502))
  case object SVC_S00_0021 extends ErrorCodeType("Service state conflict.", Some(409))
  case object SVC_S00_0022 extends ErrorCodeType("Operation not allowed.", Some(409))
  case object DAT_S00_0001 extends ErrorCodeType("Invalid request, XML format.", Some(400))
  case object DAT_S00_0002 extends ErrorCodeType("Invalid input media format.", Some(403))
  case object DAT_S00_0003 extends ErrorCodeType("Invalid jobID - the supplied jobID does not exist.", Some(404))
  case object DAT_S00_0004 extends ErrorCodeType("Missing required service metadata in request.",Some(400))
  case object DAT_S00_0005 extends ErrorCodeType("Duplicate jobID detected for new job.", Some(409))
  case object DAT_S00_0006 extends ErrorCodeType("Invalid request parameters.", Some(400))
  case object DAT_S00_0007 extends ErrorCodeType("Job command not valid.",Some(403))
  case object DAT_S00_0008 extends ErrorCodeType("Queue command not valid.", Some(403))
  case object DAT_S00_0009 extends ErrorCodeType("Invalid priority.", Some(403))
  case object DAT_S00_0010 extends ErrorCodeType("Input media not found. Invalid resource URI specified.", Some(400))
  case object DAT_S00_0011 extends ErrorCodeType("Duplicate resource.", Some(409))
  case object DAT_S00_0012 extends ErrorCodeType("Invalid resource.",Some(404))
  case object DAT_S00_0013 extends ErrorCodeType("Invalid identifier.", Some(400) )
  case object DAT_S00_0014 extends ErrorCodeType("Insufficient data.", Some(400))
  case object EXT_S00_0000 extends ErrorCodeType("Extended code. See extended error code for details.",None)
  case object SEC_S00_0001 extends ErrorCodeType("Invalid credential.", Some(403))
  case object SEC_S00_0002 extends ErrorCodeType("Credential required.", Some(401))
  case object SEC_S00_0003 extends ErrorCodeType("Insufficient permission.", Some(403))
  case object SEC_S00_0004 extends ErrorCodeType("Invalid token.",Some(403))
  case object SEC_S00_0005 extends ErrorCodeType("Missing token.", Some(400))
  case object SEC_S00_0006 extends ErrorCodeType("Resource locked.", Some(409))
}

abstract class FimsException(errorCode: ErrorCodeType) extends RuntimeException {
  override def getMessage() = errorCode.description
}

class SystemUnavailable extends FimsException(ErrorCodeType.INF_S00_0001)
class SystemTimeout extends FimsException (ErrorCodeType.INF_S00_0002 )
class SystemInternalError extends FimsException(ErrorCodeType.INF_S00_0003)
class DatabaseConnectionError extends FimsException(ErrorCodeType.INF_S00_0004)
class SystemOutOfMemory extends FimsException (ErrorCodeType.INF_S00_0005)
class SystemOutOfDiskSpace extends FimsException (ErrorCodeType.INF_S00_0006)
class MaximumConnectionsReached extends FimsException (ErrorCodeType.INF_S00_0007)
class JobCommandNotSupported extends FimsException(ErrorCodeType.SVC_S00_0001)
class QueueCommandNotSupported extends FimsException(ErrorCodeType.SVC_S00_0002)
class OperationRequestedIsNotSupported extends FimsException(ErrorCodeType.SVC_S00_0003)
class ServiceUnableToFindEndpoint extends FimsException(ErrorCodeType.SVC_S00_0004)
class JobCommandFailed extends FimsException(ErrorCodeType.SVC_S00_0005)
class QueueCommandFailed extends FimsException(ErrorCodeType.SVC_S00_0006)
class ServiceUnableToConnectToEndpoint extends FimsException (ErrorCodeType.SVC_S00_0007)
class NoNewJobsAreBeingAccepted extends FimsException(ErrorCodeType.SVC_S00_0008)
class JobEndedWithFailure extends FimsException(ErrorCodeType.SVC_S00_0009)
class ServiceReceivedNoResponse extends FimsException(ErrorCodeType.SVC_S00_0010)
class ServiceReceivedException extends FimsException(ErrorCodeType.SVC_S00_0011)
class ServiceReceivedError extends FimsException(ErrorCodeType.SVC_S00_0012)
class UnableToConnectReplyTo extends FimsException(ErrorCodeType.SVC_S00_0013)
class UnableToConnectFaultTo extends FimsException(ErrorCodeType.SVC_S00_0014)
class FeatureNotSupported extends FimsException(ErrorCodeType.SVC_S00_0015)
class DeadlinePassed extends FimsException(ErrorCodeType.SVC_S00_0016)
class ImpossibleTimeConstraints extends FimsException(ErrorCodeType.SVC_S00_0017)
class InternalOrUnknownError extends FimsException(ErrorCodeType.SVC_S00_0018)
class VersionMismatch extends FimsException(ErrorCodeType.SVC_S00_0019)
class LicenseExpired extends FimsException(ErrorCodeType.SVC_S00_0020)
class ServiceStateConflict extends FimsException(ErrorCodeType.SVC_S00_0021)
class OperationNotAllowed extends FimsException(ErrorCodeType.SVC_S00_0022)
class InvalidRequestXMLFormat extends FimsException(ErrorCodeType.DAT_S00_0001)
class InvalidInputMediaFormat extends FimsException(ErrorCodeType.DAT_S00_0002)
class InvalidJobID extends FimsException(ErrorCodeType.DAT_S00_0003)
class MissingRequiredRequest extends FimsException(ErrorCodeType.DAT_S00_0004)
class DuplicateJobIDDetected extends FimsException(ErrorCodeType.DAT_S00_0005)
class InvalidRequestParameters extends FimsException(ErrorCodeType.DAT_S00_0006)
class JobCommandNotValid extends FimsException(ErrorCodeType.DAT_S00_0007)
class QueueCommandNotValid extends FimsException(ErrorCodeType.DAT_S00_0008)
class InvalidPriority extends FimsException(ErrorCodeType.DAT_S00_0009)
class InputMediaNotFound extends FimsException(ErrorCodeType.DAT_S00_0010)
class DuplicateResource extends FimsException(ErrorCodeType.DAT_S00_0011)
class InvalidResource extends FimsException(ErrorCodeType.DAT_S00_0012)
class InvalidIdentifier extends FimsException(ErrorCodeType.DAT_S00_0013)
class InsufficientData extends FimsException(ErrorCodeType.DAT_S00_0014)
class ExtendedCode extends FimsException(ErrorCodeType.EXT_S00_0000)
class InvalidCredential extends FimsException(ErrorCodeType.SEC_S00_0001)
class CredentialRequired extends FimsException(ErrorCodeType.SEC_S00_0002)
class InsufficientPermission extends FimsException(ErrorCodeType.SEC_S00_0003)
class InvalidToken extends FimsException(ErrorCodeType.SEC_S00_0004)
class MissingToken extends FimsException(ErrorCodeType.SEC_S00_0005)
class ResourceLocked extends FimsException(ErrorCodeType.SEC_S00_0006)