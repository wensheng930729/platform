package com.bee.platform.user;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.common.enums.Status;
import com.bee.platform.user.authority.dao.mapper.AuthEnterpriseRoleMapper;
import com.bee.platform.user.authority.dao.mapper.AuthRoleMapper;
import com.bee.platform.user.authority.dao.mapper.AuthUserRoleMapper;
import com.bee.platform.user.authority.entity.AuthEnterpriseRole;
import com.bee.platform.user.authority.entity.AuthRole;
import com.bee.platform.user.authority.entity.AuthUserRole;
import com.bee.platform.user.authority.service.AuthEnterpriseService;
import com.bee.platform.user.authority.service.AuthFunctionRoleService;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import javax.servlet.http.HttpServletRequest;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.stream.Collector;
import java.util.stream.Collectors;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-03-05 18:03
 **/
@Slf4j
@RunWith(SpringRunner.class)
@SpringBootTest(classes={Application.class})
public class UserTest {

    @Autowired
    private HttpServletRequest request;
    @Autowired
    private AuthFunctionRoleService functionRoleService;
    @Autowired
    private AuthEnterpriseService enterpriseService;
    @Autowired
    private AuthEnterpriseRoleMapper enterpriseRoleMapper;
    @Autowired
    private AuthUserRoleMapper userRoleMapper;

    @Autowired
    private AuthRoleMapper roleMapper;

    @Test
    public void tree(){
        String file="head.jpg";
        log.error("后缀:{}",request.getContextPath());
    }

    @Test
    public void test() {
        /*List<AuthUserRole> userRoles = userRoleMapper.selectList(new EntityWrapper<>(new AuthUserRole().setDeleted(0).setRoleId(301)));
        for (AuthUserRole userRole : userRoles) {
            userRoleMapper.insert(new AuthUserRole()
                    .setUserId(userRole.getUserId())
                    .setRoleId(506).setPid(301)
                    .setEnterpriseId(userRole.getEnterpriseId())
                    .setLevel(4)
                    .setRoleType("base")
                    .setStatus(1)
                    .setCreateUser(1000003)
                    .setCreateTime(new Date())
                    .setDeleted(0));
        }*/

        /*List<AuthResource> resources = resourceMapper.selectList(new EntityWrapper<>(new AuthResource().setDeleted(0)
                .setSubSys("bee_industrial_brain")));
        for (AuthResource resource : resources){
            resource.setId(resource.getId() + 211);
            if (resource.getPid() != 0){
                resource.setPid(resource.getPid() + 211);
            }
            resource.setSubSys("bee_industrial_brain_si");
        }
        resourceMapper.insertAll(resources);*/

        List<AuthRole> roles = roleMapper.selectList(new EntityWrapper<>(new AuthRole().setDeleted(0)
                .setSubSys("bee_industrial_brain")));
        for (AuthRole role : roles){
            if (role.getRoleName().equals("金蜜工业大脑")){
                role.setRoleName("金蜜工业大脑-硅系");
            }
            role.setId(role.getId() + 260);
            role.setSubSys("bee_industrial_brain_si");
            roleMapper.insert(role);
        }
    }
}
