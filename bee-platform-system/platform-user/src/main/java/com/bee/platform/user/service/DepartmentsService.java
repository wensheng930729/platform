package com.bee.platform.user.service;

import com.baomidou.mybatisplus.service.IService;
import com.bee.platform.business.dto.DepartmentDTO;
import com.bee.platform.business.dto.DepartmentListDTO;
import com.bee.platform.business.dto.DepartmentTreePostDTO;
import com.bee.platform.business.rq.DepartmentAddRQ;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.DepartmentInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.user.authority.dto.AuthUsergroupDeparmentTreeDTO;
import com.bee.platform.user.authority.dto.AuthUsergroupUserListDTO;
import com.bee.platform.user.dto.DepartmentTreeDTO;
import com.bee.platform.user.entity.Departments;

import javax.servlet.http.HttpServletRequest;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;


/**
 * <p>
 *  服务类
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */
public interface DepartmentsService extends IService<Departments> {

    /**
     * @notes 通过用户id和企业id查询用户所在部门
     * @Author junyang.li
     * @Date 9:18 2019/3/6
     **/
    DepartmentInfo selectByUserIdAndOrgId(Integer userId, Integer orgId);

    /**
     * 添加部门
     * @param userInfo
     * @return
     */
    ResponseResult editDepartment(AuthPlatformUserInfo userInfo, DepartmentAddRQ rq);

    /**
     * 删除部门
     * @param userInfo
     * @param departmentId
     * @return
     */
    ResponseResult deleteDepartment(AuthPlatformUserInfo userInfo, Integer departmentId);

    /**
     * 部门添加用户
     * @param org_id
     * @param dep_ids
     * @param nickname
     * @param phone
     * @param post
     * @return
     */
    ResponseResult addUserForDepartment(int org_id, int[] dep_ids, String nickname, String phone, String post);

    /**
     * 多部门添加角色
     * @param org_id
     * @param dep_ids
     * @param nickname
     * @param phone
     * @param post
     * @return
     */
    ResponseResult addUserToMultiDep(int org_id, int[] dep_ids, String nickname, String phone, String post);

    /**
     * 获取部门数
     * @param org_id
     * @return
     */
    ResponseResult<Map<String,Integer>> getDepartmentCount(int org_id);

    /**
     * 获取企业下的部门列表
     * @param userInfo
     * @param org_id
     * @param department_id
     * @return
     */
    ResponseResult<List<Departments>> getDepartmentForOrg(AuthPlatformUserInfo userInfo, int org_id, int department_id);

    /**
     * 获取企业下的一级部门列表
     * @param userInfo
     * @param org_id
     * @return
     */
    ResponseResult<List<HashMap<String, Object>>> getOneStepDepartmentForOrg(AuthPlatformUserInfo userInfo, int org_id);

    /**
     * 获取聊天时的所有企业一级部门树
     * @param userInfo
     * @return
     */
    ResponseResult<List<Object>> getOneStepDepartmentFromOrgForIM(AuthPlatformUserInfo userInfo);

    /**
     * 获取部门下的用户
     * @param userInfo
     * @param org_id
     * @param id
     * @param page
     * @param
     * @return
     */
    ResponseResult<Map<String, Object>> getUserListForDepartment(AuthPlatformUserInfo userInfo, int org_id, int id, Page page);

    /**
     * 获取企业下的用户详细信息
     * @param userInfo
     * @param org_id
     * @param id
     * @param
     * @return
     */
    ResponseResult<Map<String, Object>> getUserInfoForOrg(AuthPlatformUserInfo userInfo, int org_id, int id);

    /**
     * 获取企业下所有用户
     * @param userInfo
     * @param page
     * @param
     * @return
     */
    ResponseResult<Map<String, Object>> getAllUserForEnterprise(AuthPlatformUserInfo userInfo, Page page);

    /**
     * 获取企业下所有已激活的用户
     * @param userInfo
     * @return
     */
    ResponseResult<List<HashMap<String, Object>>> getActivedUser(AuthPlatformUserInfo userInfo);

    /**
     * 修改部门名称
     * @param id
     * @param name
     * @return
     */
    ResponseResult changeDepartmentName(int id, String name);

    /**
     * 获取部门信息
     * @param org_id
     * @param id
     * @return
     */
    ResponseResult<HashMap<String, Object>> getDepartmentInfo(int org_id, int id);

    /**
     * 邀请单个用户
     * @param id
     * @return
     */
    ResponseResult inviteUser(HttpServletRequest request, int id);

    /**
     * 批量邀请用户
     * @param request
     * @param user_ids
     * @return
     */
    ResponseResult inviteUser(HttpServletRequest request, int[] user_ids);

    /**
     * 移除用户与部门关系
     * @param userInfo
     * @param dep_id
     * @param user_id
     * @return
     */
    ResponseResult removeUser(AuthPlatformUserInfo userInfo, int dep_id, int user_id);

    /**
     * 修改员工信息
     * @param org_id
     * @param id
     * @param dep_ids
     * @param nickname
     * @param phone
     * @param post
     * @return
     */
    ResponseResult modifyUserInfo(int org_id, int id, int[] dep_ids, String nickname, String phone, String post);

    /**
     * 返回群聊状态
     * @param ids
     * @return
     */
    ResponseResult ToDetermineWhetherTheSameBusiness(int[] ids);

    /**
     * 获取部门层级树
     * @param userInfo
     * @param level
     * @return
     */
    ResponseResult<List<DepartmentDTO>> getDepartmentTree(AuthPlatformUserInfo userInfo, Integer level);

    /**
     * 部门层级树 职位用
     * @param userInfo
     * @return
     */
    ResponseResult<List<DepartmentTreePostDTO>> getDepartmentTreeForPost(AuthPlatformUserInfo userInfo);

    /**
     * 条件获取部门列表
     * @param userInfo
     * @param name
     * @return
     */
    ResponseResult<List<DepartmentListDTO>> getDepartmentList(AuthPlatformUserInfo userInfo, String name, Page page);

    /**
     * 部门验证名称唯一性
     * @param userInfo
     * @param name
     * @param level
     * @return
     */
    ResponseResult<Integer> departmentNameCheck(AuthPlatformUserInfo userInfo, String name, Integer level, Integer departmentId, Integer treeId);

    /**
     * 根据企业id获取企业下部门信息
     * @param enterpriseId
     * @param userInfo
     * @return
     */
    ResponseResult<List<Departments>> getByEnterpriseId(Integer enterpriseId, AuthPlatformUserInfo userInfo);
    /**
     * @notes: 根据企业id查询部门，并且返回一棵树
     * @Author: junyang.li
     * @Date: 16:20 2019/6/26
     * @param enterpriseId :
     * @return: java.util.List<com.bee.platform.user.dto.DepartmentTreeDTO>
     */
    List<DepartmentTreeDTO> departmentTree(Integer enterpriseId, AuthPlatformUserInfo userInfo);

    /**
     * 根据企业id查询部门树形结构
     * @return
     */
    ResponseResult<List<DepartmentTreeDTO>> getEnterpriseDepartmentTree(Integer enterpriseId);
    /**
     * @notes: 初始化数据时调整Departments表中的level字段
     * @Author: junyang.li
     * @Date: 15:06 2019/6/3
     * @return: void
     */
    void updateLevelByIds(List<Departments> list);

    /**
     * 根据部门id查询 父级部门
     * @param id
     * @return
     */
    List<Departments> getParentDaparment(Integer id);

    ResponseResult add(AuthPlatformUserInfo userInfo, DepartmentAddRQ rq);

    /**
     * 获取用户在指定公司下的下属id集合
     * @param userId
     * @param orgId
     * @return
     */
    ResponseResult<Set<Integer>> getSubordinates(Integer userId, Integer orgId);

    /**-------------------添加用户组新增接口------------------------*/
    /**
     * 用户组--部门树
     * @param orgId
     * @return
     */
    public ResponseResult<AuthUsergroupDeparmentTreeDTO> getUsergroupDeparmentTree(int orgId);

    /**
     * 用户组--根据部门id查询用户
     * @param orgId
     * @param  departmentId
     * @return
     */
    public ResponseResult<List<AuthUsergroupUserListDTO>> getUsergroupDeparmentUsers(int orgId, int departmentId);

    /**
     * 根据公司id查询当前部门及父级部门
     * @param departmentId
     * @param orgId
     * @return
     */
    public List<Departments> getParentAndSelf(Integer departmentId,Integer orgId);

    /**-------------------添加用户组新增接口------------------------*/
}
