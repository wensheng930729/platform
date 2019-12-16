package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.user.authority.dao.mapper.AuthUserRoleBackMapper;
import com.bee.platform.user.authority.dto.BackUserRelationedRoleDTO;
import com.bee.platform.user.authority.entity.AuthUserRoleBack;
import com.bee.platform.user.authority.rq.BackUserRelationRoleRQ;
import com.bee.platform.user.authority.service.AuthUserRoleBackService;
import com.google.common.collect.Lists;
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
 * 后台用户与角色/功能/应用的关联表 服务实现类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-08-09
 */

@Slf4j
@Service
public class AuthUserRoleBackServiceImpl extends ServiceImpl<AuthUserRoleBackMapper, AuthUserRoleBack> implements AuthUserRoleBackService {

    @Transactional(rollbackFor = Exception.class)
    @Override
    public void assignPermissionsToBackUser(Integer userId, List<BackUserRelationRoleRQ> rq) {
        // 如果没有后台用户id 则不能进行操作
        if (ObjectUtils.isEmpty(userId)) {
            log.error("编辑后台用户权限失败，后台用户id为空");
            throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_BACK_SAVE_FAILED_NO_USER_ID);
        }
        Date time = new Date();
        List<String> roleType = Lists.newArrayList("custom", "enterprise_admin", "super_admin");
        Wrapper<AuthUserRoleBack> wrapper = new EntityWrapper<AuthUserRoleBack>().eq("user_id", userId).eq("deleted", 0).notIn("role_type", roleType);
        // 查询用户下是否有分配过的旧权限
        List<AuthUserRoleBack> userRoleBackList = selectList(wrapper);
        // 有旧的权限 删除旧数据
        if (!CollectionUtils.isEmpty(userRoleBackList)) {
            AuthUserRoleBack userRoleBack = new AuthUserRoleBack().setDeleted(1).setUpdateTime(time);
            if (!update(userRoleBack, wrapper)) {
                log.error("给后台用户分配权限,删除旧数据失败，调用{}的{}方法出错", "AuthUserRoleBackServiceImpl", "assignPermissionsToBackUser()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_BACK_SAVE_FAILED_DELETED_OLD_FAILED);
            }
        }
        // rq为空则不进行操作
        if (!CollectionUtils.isEmpty(rq)) {
            for (BackUserRelationRoleRQ ur : rq) {
                if (ObjectUtils.isEmpty(ur.getUserId()) || ObjectUtils.isEmpty(ur.getRoleId()) || ObjectUtils.isEmpty(ur.getPid()) || ObjectUtils.isEmpty(ur.getRoleType()) || ObjectUtils.isEmpty(ur.getFlag())|| ObjectUtils.isEmpty(ur.getSubSys())) {
                    log.error("给后台用户分配权限,请求参数不全-必须包含用户id、角色id、pid、roleType、SubSys、flag ，调用{}的{}方法出错", "AuthUserRoleServiceImpl", "assignPermissions()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_BACK_SAVE_FAILED_ERROR_P);
                }
            }

            List<AuthUserRoleBack> add = BeanUtils.assemble(AuthUserRoleBack.class, rq);
            List<AuthUserRoleBack> authUserRoleBacks = add.stream().map(o -> o.setCreateTime(time)).collect(Collectors.toList());
            if (!insertBatch(authUserRoleBacks)) {
                log.error("给后台用户分配权限保存失败，调用{}的{}方法出错", "AuthUserRoleBackServiceImpl", "assignPermissionsToBackUser()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USER_ROLE_BACK_SAVE_FAILED);
            }
        } else {

            log.info("rq为空，进行删除操作完成----后台用户id为：" + userId);
        }

        log.info("编辑用户权限完成---后台用户id为：" + userId);


    }


    @Override
    public List<BackUserRelationedRoleDTO> getUserRoleBackList(Integer userId) {
        List<String> roleType = Lists.newArrayList("custom", "enterprise_admin", "super_admin");
        List<AuthUserRoleBack> authUserRoleBacks = selectList(new EntityWrapper<AuthUserRoleBack>().eq("deleted", 0)
                .eq("user_id", userId).notIn("role_type", roleType).eq("flag", 1));
        return BeanUtils.assemble(BackUserRelationedRoleDTO.class, authUserRoleBacks);
    }
}
