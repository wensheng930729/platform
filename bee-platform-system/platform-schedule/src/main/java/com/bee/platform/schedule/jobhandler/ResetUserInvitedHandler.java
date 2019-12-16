package com.bee.platform.schedule.jobhandler;

import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.service.feign.UserInfoFeignClient;
import com.xxl.job.core.biz.model.ReturnT;
import com.xxl.job.core.handler.IJobHandler;
import com.xxl.job.core.handler.annotation.JobHandler;
import com.xxl.job.core.log.XxlJobLogger;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * @description: 每天0点重置用户邀请状态
 * @author: junyang.li
 * @create: 2019-03-20 16:40
 **/
@Slf4j
@JobHandler(value="resetUserInvitedHandler")
@Component
public class ResetUserInvitedHandler extends IJobHandler {

    @Autowired
    private UserInfoFeignClient userInfoFeignClient;

    @Override
    public ReturnT<String> execute(String param) throws Exception {
        XxlJobLogger.log("每天0点开始重置用户邀请状态");
        log.info("每天0点开始重置用户邀请状态");
        // 测试Feign客户端的调用
        ResponseResult<ResCodeEnum> result = userInfoFeignClient.resetUserInvited();
        log.info("userBeatResult=" + result);
        if(ResCodeEnum.SUCCESS.getCode().equals(result.getCode())){
            return SUCCESS;
        }
        log.error("每天0点开始重置用户邀请调用远程失败,异常信息是:{}",result);
        return FAIL;
    }
}
