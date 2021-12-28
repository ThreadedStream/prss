from .serializers import StatisticsSerializer


class SaveStatisticsView(CreateAPIView):
    serializer_class = StatisticsSerializer
    permission_classes = (AllowAny,)

    def post(self, request, *args, **kwargs):
        cost = 0
        # Avoid division by zero
        views = 1
        clicks = 1
        data = {}
        if 'date' not in request.data:
            data['success'] = False
            data['result'] = "Missing mandatory field date"
            return Response(data=data, status=status.HTTP_400_BAD_REQUEST)
        else:
            date = request.data['date']
        if 'views' in request.data:
            views = int(request.data['views'])
        if 'clicks' in request.data:
            clicks = int(request.data['clicks'])
        if 'cost' in request.data:
            cost = cost_to_float(request.data['cost'])

        s_data = {'date': date, 'views': views, 'clicks': clicks, 'cost': cost}
        s = StatisticsSerializer(data=s_data)
        if s.is_valid():
            model = s.save()
            data['success'] = True
            data['result'] = "OK"
            return Response(data=data, status=status.HTTP_201_CREATED)
        else:
            data['success'] = False
            data['result'] = s.errors
            return Response(data=data, status=status.HTTP_400_BAD_REQUEST)
