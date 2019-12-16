package com.bee.platform.user.authority.service.impl;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.mapper.Wrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.user.authority.dao.mapper.AuthUsergroupUserMapper;
import com.bee.platform.user.authority.dto.AuthUsergroupUserListDTO;
import com.bee.platform.user.authority.entity.AuthUsergroupUser;
import com.bee.platform.user.authority.service.AuthUsergroupUserService;
import com.google.common.collect.Lists;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * <p>
 * 用户组和user关联表 服务实现类
 * </p>
 *
 * @author Raphael.dq123
 * @since 2019-07-17
 */

@Slf4j
@Service
public class AuthUsergroupUserServiceImpl extends ServiceImpl<AuthUsergroupUserMapper, AuthUsergroupUser> implements AuthUsergroupUserService {

    /**
     * 用户组关联用户
     *
     * @param usergroupId 用户组id
     * @param userIds     用户id集合
     */
    @Transactional(rollbackFor = Exception.class)
    @Override
    public void usergroupRalationUser(Integer usergroupId, List<Integer> userIds) {
        Wrapper<AuthUsergroupUser> wrapper = new EntityWrapper<AuthUsergroupUser>().eq("usergroup_id", usergroupId).eq("deleted", 0);
        // 查询用户组下是否有分配过的用户
        List<AuthUsergroupUser> usergroupUserList = selectList(wrapper);
        // 有旧的用户 删除旧数据
        if (!CollectionUtils.isEmpty(usergroupUserList)) {
            AuthUsergroupUser authUsergroupUser = new AuthUsergroupUser().setDeleted(1);
            if (!update(authUsergroupUser,wrapper)) {
                log.info("给用户组分配用户,删除旧数据失败，调用{}的{}方法出错", "AuthUsergroupUserServiceImpl", "usergroupRalationUser()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_USER_SAVE_FAILED_DELETED_OLD_FAILED);
            }
        }
        // userIds为空则不进行操作
        if (!CollectionUtils.isEmpty(userIds)) {
            List<AuthUsergroupUser> list = Lists.newArrayList();
            for (Integer uId : userIds) {
                AuthUsergroupUser authUsergroupUser = new AuthUsergroupUser().setUsergroupId(usergroupId).setUserId(uId);
                list.add(authUsergroupUser);
            }
            if (!insertBatch(list)) {
                log.info("给用户组分配用户保存失败，调用{}的{}方法出错", "AuthUsergroupUserServiceImpl", "usergroupRalationUser()");
                throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.USERGROUP_USER_SAVE_FAILED);
            }
        } else {
            log.info("rq为空，进行删除操作完成----用户组id为：" + usergroupId);
        }
        log.info("编辑用户组用户完成---用户组id为：" + usergroupId);
    }

    /**
     * 根据用户组id查询用户列表
     *
     * @param usergroupId 用户组id
     * @return 用户列表
     */
    @Override
    public List<AuthUsergroupUserListDTO> getUsergroupUserList(Integer usergroupId) {
        return baseMapper.getUsergroupUserList(usergroupId);
    }
}
