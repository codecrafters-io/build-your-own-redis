import NIO

final class EchoHandler: ChannelInboundHandler {

    typealias InboundIn = ByteBuffer
    typealias OutboundOut = ByteBuffer

    func channelRead(
        context: ChannelHandlerContext,
        data: NIOAny
    ) {
        let input = self.unwrapInboundIn(data)
        guard
            let message = input.getString(at: 0, length: input.readableBytes)
        else {
            return
        }

        var buff = context.channel.allocator.buffer(capacity: message.count)
        buff.writeString(message)
        context.write(wrapOutboundOut(buff), promise: nil)
    }


    func channelReadComplete(
        context: ChannelHandlerContext
    ) {
        context.flush()
    }

    func errorCaught(
        context: ChannelHandlerContext,
        error: Error
    ) {
        print(error)

        context.close(promise: nil)
    }
}
