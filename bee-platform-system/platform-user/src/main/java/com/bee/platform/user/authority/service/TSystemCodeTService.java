package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.SystemCodeDTO;
import com.bee.platform.user.authority.entity.TSystemCodeT;
import com.bee.platform.user.authority.rq.SubSystemRQ;
import com.bee.platform.user.authority.rq.SystemCodeQueryRQ;

import java.util.List;

/**
 * <p>
 * 系统码表 服务类
 * </p>
 *
 * @author chenjie123123
 * @since 2019-05-27
 */
public interface TSystemCodeTService extends IService<TSystemCodeT> {

    /**
     * 分页查询子系统列表
     * @param pagination
     * @return
     */
    List<SystemCodeDTO> listSubSystem(SystemCodeQueryRQ rq, Pagination pagination);

    /**
     * 添加子系统
     * @param userInfo
     * @param rq
     * @return
     */
    ResponseResult<ResCodeEnum> addSubSystem(AuthPlatformUserInfo userInfo, SubSystemRQ rq);

    /**
     * 删除子系统
     * @param userInfo
     * @param id
     * @return
     */
    ResponseResult<ResCodeEnum> deleteSubSystem(AuthPlatformUserInfo userInfo, String id);
}
