package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.authority.dao.mapper.AuthUsergroupRoleMapper;
import com.bee.platform.user.authority.entity.AuthUsergroupRole;
import com.bee.platform.user.authority.rq.UsergroupRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthUsergroupRoleService;
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
public class AuthUsergroupRoleServiceImpl extends ServiceImpl<AuthUsergroupRoleMapper, AuthUsergroupRole> implements AuthUsergroupRoleService {

    /**
     * 给用户组授权
     *
     * @param usergroupId  用户组id
     * @param enterpriseId 企业id
     * @param rq           权限列表
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void assignPermissions(Integer usergroupId, Integer enterpriseId, List<UsergroupRelationRoleRQ> rq) {
        // 如果没有用户组id 则不能进行操作
        if (ObjectUtils.isEmpty(usergroupId) || ObjectUtils.isEmpty(enterpriseId)) {
            log.info("编辑用户组权限失败，用户组id为空");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_ROLE_SAVE_FAILED_NO_USERGROUP_ID);
        }
        Date time = new Date();
        Wrapper<AuthUsergroupRole> wrapper = new EntityWrapper<AuthUsergroupRole>().eq("usergroup_id", usergroupId).eq("enterprise_id", enterpriseId).eq("deleted", 0);
        // 查询用户组下是否有分配过的旧权限
        List<AuthUsergroupRole> userRoleList = selectList(wrapper);
        // 有旧的权限 删除旧数据
        if (!CollectionUtils.isEmpty(userRoleList)) {
            AuthUsergroupRole usergroupRole = new AuthUsergroupRole().setDeleted(1).setUpdateTime(time);
            if (!update(usergroupRole, wrapper)) {
                log.info("给用户组分配权限,删除旧数据失败，调用{}的{}方法出错", "AuthUsergroupRoleServiceImpl", "assignPermissions()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_ROLE_SAVE_FAILED_DELETED_OLD_FAILED);
            }
        }
        // rq为空则不进行操作
        if (!CollectionUtils.isEmpty(rq)) {
            for (UsergroupRelationRoleRQ ur : rq) {
                if (ObjectUtils.isEmpty(ur.getUsergroupId()) || ObjectUtils.isEmpty(ur.getEnterpriseId())
                        || ObjectUtils.isEmpty(ur.getRoleId()) || ObjectUtils.isEmpty(ur.getPid()) || ObjectUtils.isEmpty(ur.getRoleType()) || ObjectUtils.isEmpty(ur.getSubSys())|| ObjectUtils.isEmpty(ur.getFlag())
                ) {
                    log.info("给用户组分配权限,请求参数不全-必须包含用户组id、企业id、角色id、pid、roleType、subSys、flag ，调用{}的{}方法出错", "AuthUsergroupRoleServiceImpl", "assignPermissions()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_ROLE_SAVE_FAILED_ERROR_P);
                }
            }

            List<AuthUsergroupRole> add = BeanUtils.assemble(AuthUsergroupRole.class, rq);
            List<AuthUsergroupRole> authUsergroupRoles = add.stream().map(o -> o.setCreateTime(time)).collect(Collectors.toList());
            if (!insertBatch(authUsergroupRoles)) {
                log.info("给用户组分配权限保存失败，调用{}的{}方法出错", "AuthUsergroupRoleServiceImpl", "assignPermissions()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_ROLE_SAVE_FAILED);
            }
        } else {

            log.info("rq为空，进行删除操作完成----用户组id为：" + usergroupId);
        }

        log.info("编辑用户组权限完成---用户id为：" + usergroupId);
    }

    /**
     * 查询企业下用户组角色信息
     *
     * @param usergroupId  用户组id
     * @param enterpriseId 企业id
     * @return 角色列表
     */
    @Override
    public List<UsergroupRelationRoleRQ> getUsergroupRoleList(Integer usergroupId, Integer enterpriseId) {
        List<AuthUsergroupRole> authUsergroupRoles = selectList(new EntityWrapper<AuthUsergroupRole>().eq("deleted", 0)
                .eq("usergroup_id", usergroupId).eq("enterprise_id", enterpriseId).eq("flag",1));
        List<UsergroupRelationRoleRQ> dto = BeanUtils.assemble(UsergroupRelationRoleRQ.class, authUsergroupRoles);
        return dto;
    }
}
