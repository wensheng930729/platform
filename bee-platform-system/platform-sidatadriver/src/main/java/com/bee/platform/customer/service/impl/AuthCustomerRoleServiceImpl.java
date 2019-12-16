package com.bee.platform.customer.service.impl;

import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.customer.dao.mapper.AuthCustomerRoleMapper;
import com.bee.platform.customer.entity.AuthCustomerRole;
import com.bee.platform.customer.service.AuthCustomerRoleService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

/**
 * <p>
 * 客户关联角色的中间表 服务实现类
 * </p>
 *
 * @author
 * @since 2019-05-20
 */
@Slf4j
@Service
public class AuthCustomerRoleServiceImpl extends ServiceImpl<AuthCustomerRoleMapper, AuthCustomerRole> implements AuthCustomerRoleService {

   /* @Autowired
    private AuthRoleService authRoleService;

    *//**
     * 客户关联角色、功能、应用
     * @param createId 创建人id
     * @param rq 请求参数
     * @return 操作结果
     *//*
    @Override
    public ResponseResult changeCustomerRole(Integer createId, CustomerRelationRoleRQ rq) {

        List<Integer> childIds = rq.getRoleIds();
        Integer pId = rq.getCustomerId();
        Integer type = rq.getType();
        Date time = new Date();
        switch (type){
            case 0:
                // 添加客户与角色/功能/应用对应关系
                saveCustomerRole(createId, time, childIds, pId);
                break;
            case 1:
                // 删除客户与角色/功能/应用关联关系
                deleteCustomerRole(time, pId);
                // 添加客户与角色/功能/应用对应关系
                saveCustomerRole(createId, time, childIds, pId);
                break;
            case 2:
                // 删除客户与角色/功能/应用关联关系
                deleteCustomerRole(time, pId);
                break;
            default:
                throw new BusinessException(ResCodeEnum.FAILED, ExceptionMessageEnum.CUSTOMER_ROLE_UPDATE_FAILED);

        }

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    *//**
     * 删除客户与角色功能应用对应关系
     * @param time 修改时间
     * @param pId 父级id
     *//*
    private void deleteCustomerRole(Date time, Integer pId) {
        if(!this.update(new AuthCustomerRole().setDeleted(1).setUpdateTime(time),new EntityWrapper<AuthCustomerRole>()
                .eq("customer_id",pId).eq("deleted",0))){
            log.error("修改客户与角色、功能、应用失败，调用{}的{}方法出错","AuthCustomerRoleServiceImpl","deleteUserRole()");
            throw new BusinessException(ResCodeEnum.DELETE_FAILED, ExceptionMessageEnum.CUSTOMER_ROLE_DELETE_FAILED);
        }
    }

    *//**
     * 添加客户与角色功能应用对应关系
     * @param createId 创建人id
     * @param time 创建时间
     * @param childIds 子id集合
     * @param pId 父id
     *//*
    private void saveCustomerRole(Integer createId, Date time, List<Integer> childIds, Integer pId) {
        // 添加客户与角色/功能/应用关联关系
        for (Integer childId : childIds) {
            // 查询是否存在该角色/功能/应用
            AuthRole authRole = authRoleService.selectOne(new EntityWrapper<AuthRole>().eq("id",childId).eq("deleted",0));
            // 查询是否关联过该角色/功能/应用
            AuthCustomerRole customerRole = selectOne(new EntityWrapper<AuthCustomerRole>().eq("customer_id", pId).eq("role_id", childId).eq("deleted", 0));
            if(!ObjectUtils.isEmpty(authRole)&&ObjectUtils.isEmpty(customerRole)){
                AuthCustomerRole authCustomerRole = new AuthCustomerRole().setCustomerId(pId).setRoleId(childId).setCreateTime(time)
                        .setUpdateTime(time).setDeleted(0).setStatus(1).setCreateUser(createId);
                if(!insert(authCustomerRole)){
                    log.error("添加客户与角色、功能、应用关联失败，调用{}的{}方法出错","AuthCustomerRoleServiceImpl","saveUserRole()");
                    throw new BusinessException(ResCodeEnum.SAVE_FAILED, ExceptionMessageEnum.CUSTOMER_ROLE_SAVE_FAILED);
                }
            }
        }
    }*/
}
