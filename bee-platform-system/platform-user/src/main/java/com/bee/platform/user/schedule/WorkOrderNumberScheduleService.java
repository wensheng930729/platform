package com.bee.platform.user.schedule;

import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.dao.mapper.SequenceMapper;
import com.bee.platform.common.entity.Sequence;
import com.bee.platform.common.utils.ConstInfos;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.util.ObjectUtils;

/**
 * @ClassName WorkOrderNumberScheduleService
 * @Description 定时任务-工单编号序列号初始化
 * @author qhwang
 * @Date 2019/4/30 11:02
 * @version 1.0.0
 */
@Slf4j
@Component
public class WorkOrderNumberScheduleService {

    @Autowired
    private SequenceMapper sequenceMapper;

    /**
     * 每天凌晨0点30分启动
     */
    @Scheduled(cron = "0 30 0 * * ?")
    public void initWorkOrderNumber() {
        //查询工单
        Sequence sequence = sequenceMapper.selectOne(new Sequence().setSequenceKey(
                ConstInfos.Sequence.workOrdersSeq.getKey()).setStatus(EnumCommon.IsActive.is_active.getKey()));
        if (!ObjectUtils.isEmpty(sequence)) {
            //初始化序列号
            sequence.setSequenceValue(11000);
            sequenceMapper.updateById(sequence);
        }
    }

}
