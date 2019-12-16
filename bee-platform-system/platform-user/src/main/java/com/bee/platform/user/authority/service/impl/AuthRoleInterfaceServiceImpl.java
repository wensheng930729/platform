package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.authority.dao.mapper.AuthRoleInterfaceMapper;
import com.bee.platform.user.authority.entity.AuthRoleInterface;
import com.bee.platform.user.authority.rq.AuthInterfaceRoleRQ;
import com.bee.platform.user.authority.service.AuthRoleInterfaceService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author jie.chen123
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthRoleInterfaceServiceImpl extends ServiceImpl<AuthRoleInterfaceMapper, AuthRoleInterface> implements AuthRoleInterfaceService {

    @Autowired
    private AuthRoleInterfaceMapper authRoleInterfaceMapper;

    /**
     * @Description 查询角色已绑定的接口
     * @Param roleId
     * @Date 2019/5/21 15:31
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<List<AuthRoleInterface>> findList(Integer roleId, Page page) {
        Pagination pagination= PageUtils.transFromPage(page);
        EntityWrapper<AuthRoleInterface> wrapper = new EntityWrapper<>();
        wrapper.eq("role_id", roleId);
        wrapper.eq("status", Status.TRUE.getKey().toString());
        wrapper.eq("deleted", Status.FALSE.getKey().toString());
        List<AuthRoleInterface> result = authRoleInterfaceMapper.selectPage(pagination, wrapper);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, result, PageUtils.transToPage(pagination));
    }

    /**
     * @Description 角色绑定接口
     * @Param authInterfaceRoleRQs
     * @Date 2019/5/21 14:29
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> add(List<AuthInterfaceRoleRQ> authInterfaceRoleRQs) {
        List<AuthRoleInterface> authRoleInterfaces = new ArrayList<AuthRoleInterface>();
        for (AuthInterfaceRoleRQ authInterfaceRoleRQ : authInterfaceRoleRQs) {
            authRoleInterfaces.add(BeanUtils.copyProperties(authInterfaceRoleRQ,
                    AuthRoleInterface.class).setCreateTime(new Date())
                    .setStatus(Status.TRUE.getKey()));
        }
        if (authRoleInterfaceMapper.batchInsert(authRoleInterfaces) < 0) {
            log.error("角色添加接口失败，调用方法{}", "AuthRoleInterfaceServiceImpl.add()");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 更新角色绑定的接口
     * @Param authInterfaceRoleRQs
     * @Date 2019/5/21 15:06
     * @Author xin.huang
     * @Return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> update(List<AuthInterfaceRoleRQ> authInterfaceRoleRQs) {
        Integer deleteInterface = authRoleInterfaceMapper.update(new AuthRoleInterface()
                        .setUpdateTime(new Date()).setDeleted(Status.TRUE.getKey()),
                new EntityWrapper<AuthRoleInterface>()
                        .eq("role_id", authInterfaceRoleRQs.get(0).getRoleId()));
        if (deleteInterface < 0) {
            log.error("删除角色绑定的接口失败，调用方法{}", "AuthRoleInterfaceServiceImpl.update()");
            return ResponseResult.buildResponseResult(ResCodeEnum.UPDATE_FAIL);
        }
        List<AuthRoleInterface> authRoleInterfaces = new ArrayList<AuthRoleInterface>();
        for (AuthInterfaceRoleRQ authInterfaceRoleRQ : authInterfaceRoleRQs) {
            authRoleInterfaces.add(BeanUtils.copyProperties(authInterfaceRoleRQ,
                    AuthRoleInterface.class).setCreateTime(new Date())
                    .setStatus(Status.TRUE.getKey()));
        }
        if (authRoleInterfaceMapper.batchInsert(authRoleInterfaces) < 0) {
            log.error("更新角色绑定的接口失败，调用方法{}", "AuthRoleInterfaceServiceImpl.update()");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @Description 删除角色已绑定的接口
     * @Param interfaceIds
     * @Param roleId
     * @Date 2019/5/21 15:48
     * @Author xin.huang
     * @Return
     */
    @Override
    public ResponseResult<ResCodeEnum> delete(List<Integer> interfaceIds, Integer roleId) {
        Integer deleteInterface = authRoleInterfaceMapper.update(new AuthRoleInterface()
                        .setUpdateTime(new Date()).setDeleted(Status.TRUE.getKey()),
                         new EntityWrapper<AuthRoleInterface>()
                        .eq("role_id", roleId)
                        .in("interface_id", interfaceIds));
        if (deleteInterface < 0) {
            log.error("删除角色绑定的接口失败，调用方法{}", "AuthRoleInterfaceServiceImpl.delete()");
            return ResponseResult.buildResponseResult(ResCodeEnum.DELETE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }
}
