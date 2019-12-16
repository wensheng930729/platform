package com.bee.platform.datadriver.utils;

import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.DateUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.Date;

/**
 * @Classname GenerateIdUtils
 * @Description 自动生成业务id
 * @Date 2019/5/28 10:15
 * @Author xin.huang
 */
@Slf4j
@Component
public class GenerateIdUtils {

    @Autowired
    private JedisService jedisService;

    private GenerateIdUtils(){}

    public String generateStorageOrderId() {
        String time= DateUtils.format(new Date(), DateUtils.YMD);
        String key=new StringBuilder(ConstantsUtil.PURCHASE_GOODS_ID_KEY)
                .append(time)
                .append(ConstantsUtil.UNDERLINE)
                .toString();
        Integer number=jedisService.incr(key);
        String str = String.format("%03d", number);
        return new StringBuilder().append("RK").append(time).append(str).toString();
    }
}
