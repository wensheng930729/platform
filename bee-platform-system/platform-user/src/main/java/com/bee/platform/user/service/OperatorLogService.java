package com.bee.platform.user.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.OperatorLogType;
import com.bee.platform.user.dto.OperatorLogDTO;
import com.bee.platform.user.entity.OperatorLog;
import com.baomidou.mybatisplus.service.IService;

import java.util.List;

/**
 * @notes 后台管理日志操作类
 * @Author junyang.li
 * @Date 17:13 2019/4/30
 **/
public interface OperatorLogService extends IService<OperatorLog> {
    /**
     * @notes: 分页查询操作日志
     * @Author: junyang.li
     * @Date: 9:45 2019/5/5
     * @param pagination :  分页对象
     * @return: com.bee.platform.common.entity.ResponseResult
     */
    ResponseResult<List<OperatorLogDTO>> list(Pagination pagination);
    /**
     * @notes: 生成日志内容
     * @Author: junyang.li
     * @Date: 9:29 2019/5/23
     * @param logType : 日志类型
     * @param params : 参数
     * @return: java.lang.String
     */
    String createContent(OperatorLogType logType,Object...params);
    /**
     * @notes: 创建操作日志对象
     * @Author: junyang.li
     * @Date: 9:48 2019/5/5
     * @param managerInfo : 操作用户信息
     * @param content : 操作内容
     * @return: com.bee.platform.user.entity.OperatorLog
     */
    OperatorLog createOperatorLog(ManagerInfo managerInfo,String content);
    /**
     * @notes: 创建操作日志对象,并插入到数据库中
     * @Author: junyang.li
     * @Date: 9:23 2019/5/23
     * @param managerInfo : 操作用户信息
     * @param content : 操作内容
     * @return: void
     */
    void insetOperatorLog(ManagerInfo managerInfo,String content);
}
