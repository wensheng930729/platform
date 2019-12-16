package com.bee.platform.user.config;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.enums.Status;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.service.EnterprisesUsersService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.CrossOrigin;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @notes 每天0点重置用户邀请状态
 * @Author junyang.li
 * @Date 14:38 2019/3/22
 **/
@Slf4j
@Configuration
@EnableScheduling
@CrossOrigin(origins = "*")
public class SchedulingTask {

    @Autowired
    private EnterprisesUsersService enterprisesUsersService;


    /**
     * 每天0点重置用户邀请状态
     */
    @Scheduled(cron = "0 0 0 * * ?")
    public void resetUserInvited() {
        List<EnterprisesUsers> list= enterprisesUsersService.selectList(new EntityWrapper<EnterprisesUsers>()
                .where("is_active={0} and is_invite={1}", Status.FALSE.getKey(),Status.TRUE.getKey()));
        if(CollectionUtils.isEmpty(list)){
            log.info("每天0点重置用户邀请状态查询数据为空,执行完成");
            return;
        }
        List<Integer> ids=list.stream().map(EnterprisesUsers::getId).collect(Collectors.toList());
        enterprisesUsersService.updateInvite(ids);
        log.info("每天0点重置用户邀请状态成功！");
    }
}
