package com.bee.platform.user.utils;

import com.bee.platform.common.enums.BusinessIdType;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import redis.clients.jedis.exceptions.JedisConnectionException;
import redis.clients.jedis.exceptions.JedisException;

import javax.annotation.PostConstruct;
import java.util.Calendar;
import java.util.Date;

/**
 * @ClassName CenerateIdUtils
 * @Description 生成业务ID工具类
 * @Author xin.huang
 * @Date 2019/4/30 14:03
 * @Version 1.0.0
 */
@SuppressWarnings("all")
@Slf4j
@Component
public class CenerateIdUtils {

    @Autowired
    private JedisService jedisService;
    /**
     * @Description 生成业务ID
     * @Author xin.huang
     * @Param businessMode 业务类型：用户01，订单02，合同03，补充协议04，结算单05，企业06
     * @Param createId
     * @return
     **/
    public static String generateOrderId(String businessMode,Integer createId) {
        Calendar calendar = Calendar.getInstance();
        //获取当前年份的后两位
        String year = Integer.toString(calendar.get(Calendar.YEAR)).substring(2);
        // 获取到0-11，与我们正常的月份差1
        String month = (calendar.get(Calendar.MONTH) +1)< 10 ? "0"+ (calendar.get(Calendar.MONTH) +1): String.valueOf((calendar.get(Calendar.MONTH) +1));
        // 获取到0-11，与我们正常的月份差1
        String day = calendar.get(Calendar.DAY_OF_MONTH) < 10 ? "0"+calendar.get(Calendar.DAY_OF_MONTH) : String.valueOf(calendar.get(Calendar.DAY_OF_MONTH));
        //生成6位序列号
        String serialNumber = "000000";
        String userId = String.valueOf(createId);
        if (userId.length() <= 6) {
            serialNumber = serialNumber.substring(0, 6 - userId.length()) + userId;
        }

        StringBuffer applyId = new StringBuffer();
        applyId.append(year).append(month).append(day).append(serialNumber).append(businessMode);
        return applyId.toString();
    }

    /**
     * @notes: 业务id生成规则
     * @Author: junyang.li
     * @Date: 11:40 2019/5/7
     * @param type : 业务id类型枚举
     * @return: java.lang.String
     */
    public String generateOrderId(BusinessIdType type) {
        String time= DateUtils.format(new Date(),DateUtils.YMD).substring(2);
        String key=new StringBuilder(ConstantsUtil.BUSINESS_ID_KEY).append(time).append(ConstantsUtil.UNDERLINE)
                .append(type.getDesc()).toString();
        Integer number=jedisService.incr(key);
        String str = String.format("%06d", number);
        return new StringBuilder(time).append(str).append(type.getCode()).toString();
    }

    /**
     * @notes: 如果第一个方法抛出异常
     * @Author: junyang.li
     * @Date: 11:50 2019/5/7
     * @param type : 业务id类型
     * @param count : 数据库统计总数
     * @return: java.lang.String
     */
    public  String generateOrderId(BusinessIdType type,int count) {
        String time= DateUtils.format(new Date(),DateUtils.YMD).substring(2);
        count++;
        String key=new StringBuilder(ConstantsUtil.BUSINESS_ID_KEY).append(time).append(ConstantsUtil.UNDERLINE)
                .append(type.getDesc()).toString();
        String str = String.format("%06d", count);
        //插入到缓存中
        try {
            jedisService.setIncr(key,count);
        }catch (JedisException e){
            log.error("generateOrderId方法插入缓存失败，key={},value={},异常信息是:{}",key,count,e);
        }
        return new StringBuilder(time).append(str).append(type.getCode()).toString();
    }
}
