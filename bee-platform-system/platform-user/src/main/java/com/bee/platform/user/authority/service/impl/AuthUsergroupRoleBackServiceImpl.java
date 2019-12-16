package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.authority.dao.mapper.AuthUsergroupRoleBackMapper;
import com.bee.platform.user.authority.dto.UserGroupBackRelationRoleDTO;
import com.bee.platform.user.authority.entity.AuthUsergroupRoleBack;
import com.bee.platform.user.authority.rq.UserGroupBackRelationRoleRQ;
import com.bee.platform.user.authority.rq.UsergroupRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthUsergroupRoleBackService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 * 用户组与角色/功能/应用的关联表 服务实现类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */

@Slf4j
@Service
public class AuthUsergroupRoleBackServiceImpl extends ServiceImpl<AuthUsergroupRoleBackMapper, AuthUsergroupRoleBack> implements AuthUsergroupRoleBackService {

    /**
     * 给后台用户组授权
     *
     * @param userGroupId  后台用户组id
     * @param rq           权限列表
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void assignPermissionsToBackUserGroup(Integer userGroupId, List<UserGroupBackRelationRoleRQ> rq) {
        // 如果没有后台用户组id 则不能进行操作
        if (ObjectUtils.isEmpty(userGroupId) ) {
            log.info("编辑后台用户组权限失败，后台用户组id为空");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_ROLE_BACK_SAVE_FAILED_NO_USERGROUP_ID);
        }
        Date time = new Date();
        Wrapper<AuthUsergroupRoleBack> wrapper = new EntityWrapper<AuthUsergroupRoleBack>().eq("usergroup_id", userGroupId).eq("deleted", 0);
        // 查询后台用户组下是否有分配过的旧权限
        List<AuthUsergroupRoleBack> userRoleBackList = selectList(wrapper);
        // 有旧的权限 删除旧数据
        if (!CollectionUtils.isEmpty(userRoleBackList)) {
            AuthUsergroupRoleBack userGroupRoleBack = new AuthUsergroupRoleBack().setDeleted(1).setUpdateTime(time);
            if (!update(userGroupRoleBack, wrapper)) {
                log.info("给后台用户组分配权限,删除旧数据失败，调用{}的{}方法出错", "AuthUsergroupRoleBackServiceImpl", "assignPermissionsToBackUserGroup()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_ROLE_BACK_SAVE_FAILED_DELETED_OLD_FAILED);
            }
        }
        // rq为空则不进行操作
        if (!CollectionUtils.isEmpty(rq)) {
            for (UserGroupBackRelationRoleRQ ur : rq) {
                if (ObjectUtils.isEmpty(ur.getUsergroupId()) || ObjectUtils.isEmpty(ur.getRoleId()) || ObjectUtils.isEmpty(ur.getPid()) || ObjectUtils.isEmpty(ur.getRoleType()) || ObjectUtils.isEmpty(ur.getSubSys()) || ObjectUtils.isEmpty(ur.getFlag())) {
                    log.info("给后台用户组分配权限,请求参数不全-必须包含用户组id、角色id、pid、roleType、subSys、flag ，调用{}的{}方法出错", "AuthUsergroupRoleBackServiceImpl", "assignPermissionsToBackUserGroup()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_ROLE_BACK_SAVE_FAILED_ERROR_P);
                }
            }

            List<AuthUsergroupRoleBack> add = BeanUtils.assemble(AuthUsergroupRoleBack.class, rq);
            List<AuthUsergroupRoleBack> authUsergroupRoleBacks = add.stream().map(o -> o.setCreateTime(time)).collect(Collectors.toList());
            if (!insertBatch(authUsergroupRoleBacks)) {
                log.info("给后台用户组分配权限保存失败，调用{}的{}方法出错", "AuthUsergroupRoleBackServiceImpl", "assignPermissionsToBackUserGroup()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_ROLE_BACK_SAVE_FAILED);
            }
        } else {

            log.info("rq为空，进行删除操作完成----用户组id为：" + userGroupId);
        }

        log.info("编辑后台用户组权限完成---用户id为：" + userGroupId);
    }



    /**
     * 查询后台用户组角色信息
     *
     * @param userGroupId  后台用户组id
     * @return 角色列表
     */
    @Override
    public List<UserGroupBackRelationRoleDTO> getUserGroupRoleBackList(Integer userGroupId) {
        List<AuthUsergroupRoleBack> authUsergroupRoleBacks = selectList(new EntityWrapper<AuthUsergroupRoleBack>()
                .eq("deleted", 0)
                .eq("usergroup_id", userGroupId)
                .eq("flag", 1));
        return BeanUtils.assemble(UserGroupBackRelationRoleDTO.class, authUsergroupRoleBacks);
    }
}
