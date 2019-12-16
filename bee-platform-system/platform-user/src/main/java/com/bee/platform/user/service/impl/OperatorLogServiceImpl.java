package com.bee.platform.user.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.OperatorLogType;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.dao.mapper.OperatorLogMapper;
import com.bee.platform.user.dto.OperatorLogDTO;
import com.bee.platform.user.entity.OperatorLog;
import com.bee.platform.user.service.OperatorLogService;
import com.bee.platform.user.service.PlatformManagersService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * @notes 后台管理日志操作类
 * @Author junyang.li
 * @Date 17:13 2019/4/30
 **/
@Service
public class OperatorLogServiceImpl extends ServiceImpl<OperatorLogMapper, OperatorLog> implements OperatorLogService {

    @Autowired
    private OperatorLogMapper operatorLogMapper;

    @Autowired
    private PlatformManagersService platformManagersService;

    /**
     * @notes: 分页查询操作日志
     * @Author: junyang.li
     * @Date: 9:45 2019/5/5
     * @param pagination :  分页对象
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    @Override
    public ResponseResult<List<OperatorLogDTO>> list(Pagination pagination) {
        List<OperatorLog> list=operatorLogMapper.selectPage(pagination,new EntityWrapper<OperatorLog>()
                .orderBy("operator_time",false));
        if(CollectionUtils.isEmpty(list)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,new ArrayList<>());
        }
        //查询对应的用户名称
        Set<Integer> operatorIds=list.stream().map(OperatorLog::getOperatorId).collect(Collectors.toSet());
        Map<Integer,String> map= platformManagersService.getManagerNameById(operatorIds);
        //组装数据
        List<OperatorLogDTO> dtos=list.stream().map(obj->{
            return BeanUtils.copyProperties(obj,OperatorLogDTO.class).setOperatorName(map.get(obj.getOperatorId()));
        }).collect(Collectors.toList());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,dtos, PageUtils.transToPage(pagination));
    }
    /**
     * @notes: 生成日志内容
     * @Author: junyang.li
     * @Date: 9:29 2019/5/23
     * @param logType : 日志类型
     * @param params : 参数
     * @return: java.lang.String
     */
    @Override
    public String createContent(OperatorLogType logType, Object... params) {
        return MessageFormat.format(logType.getDesc(),params);
    }

    /**
     * @notes: 创建操作日志对象
     * @Author: junyang.li
     * @Date: 9:48 2019/5/5
     * @param managerInfo : 操作用户信息
     * @param content : 操作内容
     * @return: com.bee.platform.user.entity.OperatorLog
     */
    @Override
    public OperatorLog createOperatorLog(ManagerInfo managerInfo, String content) {
        //记录操作日志
        return new OperatorLog().setOperatorId(managerInfo.getManagerId()).setOperatorRoleId(managerInfo.getRoleInfo().getRoleId())
                .setOperatorRoleName(managerInfo.getRoleInfo().getRoleName()).setOperatorTime(new Date())
                .setOperatorContent(content);
    }
    /**
     * @notes: 创建操作日志对象,并插入到数据库中
     * @Author: junyang.li
     * @Date: 9:23 2019/5/23
     * @param managerInfo : 操作用户信息
     * @param content : 操作内容
     * @return: void
     */
    @Override
    public void insetOperatorLog(ManagerInfo managerInfo, String content) {
        OperatorLog log=this.createOperatorLog(managerInfo,content);
        this.insert(log);
    }
}
