package com.bee.platform.user.authority.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.entity.AuthRoleInterface;
import com.bee.platform.user.authority.rq.AuthInterfaceRoleRQ;

import java.util.List;

/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
public interface AuthRoleInterfaceService extends IService<AuthRoleInterface> {

    /**
     * @Description 查询角色已绑定的接口
     * @Param roleId
     * @Date 2019/5/21 15:31
     * @Author xin.huang
     * @Return
     */
    public ResponseResult<List<AuthRoleInterface>> findList(Integer roleId, Page page);

    /**
     * @Description 接口绑定角色
     * @Param authInterfaceRoleRQs
     * @Date 2019/5/21 14:29
     * @Author xin.huang
     * @Return
     */
    public ResponseResult<ResCodeEnum> add(List<AuthInterfaceRoleRQ> authInterfaceRoleRQs);

    /**
     * @Description 更新接口绑定的角色
     * @Param authInterfaceRoleRQs
     * @Date 2019/5/21 15:06
     * @Author xin.huang
     * @Return
     */
    public ResponseResult<ResCodeEnum> update(List<AuthInterfaceRoleRQ> authInterfaceRoleRQs);

    /**
     * @Description 删除角色已绑定的接口
     * @Param interfaceIds
     * @Param roleId
     * @Date 2019/5/21 15:48
     * @Author xin.huang
     * @Return
     */
    public ResponseResult<ResCodeEnum> delete(List<Integer> interfaceIds, Integer roleId);

}
