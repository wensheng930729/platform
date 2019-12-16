package com.bee.platform.user.service.impl;

import com.alibaba.fastjson.JSONArray;
import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.entity.MRoleInfo;
import com.bee.platform.common.entity.ManagerInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.ManagerRoleType;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.OperatorLogType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.service.JedisService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.user.dao.mapper.MManagersRolesMapper;
import com.bee.platform.user.dao.mapper.MRolesMapper;
import com.bee.platform.user.dto.MRoleDTO;
import com.bee.platform.user.dto.MRoleGroupDTO;
import com.bee.platform.user.dto.MRoleListDTO;
import com.bee.platform.user.dto.ManagerRoleCountDTO;
import com.bee.platform.user.entity.MRoleRole;
import com.bee.platform.user.entity.MRoles;
import com.bee.platform.user.service.MRoleRoleService;
import com.bee.platform.user.service.MRolesService;
import com.bee.platform.user.service.OperatorLogService;
import com.bee.platform.user.service.SystemNoticeService;
import com.bee.platform.user.vo.ManagerRoleVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.StringUtils;

import java.text.MessageFormat;
import java.util.*;
import java.util.stream.Collectors;

/**
 * <p>
 * 管理员角色表 服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-28
 */
@Slf4j
@Service
public class MRolesServiceImpl extends ServiceImpl<MRolesMapper, MRoles> implements MRolesService {

    @Autowired
    private JedisService jedisService;

    @Autowired
    private MRolesMapper rolesMapper;

    @Autowired
    private MManagersRolesMapper managersRolesMapper;

    @Autowired
    private MRoleRoleService roleRoleService;

    @Autowired
    private OperatorLogService operatorLogService;

    @Autowired
    private SystemNoticeService systemNoticeService;
    /**
     * @notes  获得所有的角色
     * @Author junyang.li
     * @Date 14:34 2019/4/29
     **/
    @Override
    public List<MRoleInfo> getAllRoles() {
        try {
            jedisService.delKey(ConstantsUtil.MANAGER_ALL_ROLE);
            String str = jedisService.get(ConstantsUtil.MANAGER_ALL_ROLE);
            if (StringUtils.isEmpty(str)) {
                List<MRoles> roles = allRoles();
                //存入缓存中
                jedisService.set(ConstantsUtil.MANAGER_ALL_ROLE, JSONObject.toJSONString(roles), ConstantsUtil.OVERDUE);
                return BeanUtils.assemble(MRoleInfo.class,roles);
            }
            return JSONArray.parseArray(str, MRoleInfo.class);
        } catch (Exception e) {
            log.error("获取所有管理后台角色异常，异常信息是:{}", e);
            List<MRoles> roles = allRoles();
            return BeanUtils.assemble(MRoleInfo.class,roles);
        }
    }

    /**
     * @notes 通过roleId查询role
     * @Author junyang.li
     * @Date 14:46 2019/4/29
     **/
    @Override
    public List<MRoleInfo> getRolesByRoleIds(List<Integer> roleIds) {
       List<MRoleInfo> list= getAllRoles();
       return list.stream().map(obj->{
            if(roleIds.contains(obj.getRoleId())){
                return obj;
            }
            return null;
        }).filter(Objects::nonNull).collect(Collectors.toList());
    }

    /**
     * @notes: 通过roleIds查询roles 返回map对象
     * @Author: junyang.li
     * @Date: 11:31 2019/5/9
     * @param roleIds :  角色id
     * @return: java.util.Map<java.lang.Integer,com.bee.platform.common.entity.MRoleInfo>
     */
    @Override
    public Map<Integer, MRoleInfo> searchRolesByRoleIds(List<Integer> roleIds) {
        List<MRoleInfo> list= getAllRoles();
        Map<Integer, MRoleInfo> map=new HashMap<>(16);
        for (MRoleInfo item:list) {
            if(roleIds.contains(item.getRoleId())){
                map.put(item.getRoleId(),item);
            }
        }
        return map;
    }

    /**
     * @notes 通过roleId查询角色
     * @Author junyang.li
     * @Date 14:46 2019/4/29
     **/
    @Override
    public MRoleInfo getRoleByRoleId(Integer roleId) {
        if(roleId==null){
            return null;
        }
        List<MRoleInfo> list= getAllRoles();
        for (MRoleInfo item:list) {
            if(item.getRoleId().equals(roleId)){
                return item;
            }
        }
        return null;
    }

    /**
     * @notes 权限配置权限列表查询
     * @Author junyang.li
     * @Date 16:32 2019/4/29
     **/
    @Override
    public ResponseResult<List<MRoleListDTO>> getPermissionGroup() {
        //普通角色就是新增的权限组
        int common=ManagerRoleType.COMMON.getKey();
        //所有的角色
        List<MRoles> roles=this.selectList(new EntityWrapper<MRoles>().where(" status =1 and role_type={0}",common));
        List<MRoleListDTO> list=new ArrayList<>();

        //遍历获得权限组名称和id
        List<Integer> roleIds=roles.stream().map(obj->{
            MRoleListDTO dto=BeanUtils.copyProperties(obj,MRoleListDTO.class);
            if(dto.getModifyId()==null){
                dto.setModifyId(obj.getCreateId());
            }
            if(dto.getModifier()==null){
                dto.setModifier(obj.getCreator());
            }
            list.add(dto);
            return obj.getRoleId();
        }).collect(Collectors.toList());
        //判空
        if(CollectionUtils.isEmpty(roleIds)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
        }
        //查出每个权限组下的账号数量
        List<ManagerRoleCountDTO> countDTOS=managersRolesMapper.countManagerByRoleIds(roleIds);
        //将查出的数据转成map对象方便获取数据
        Map<Integer,Integer> map=countDTOS.stream().collect(Collectors.toMap(ManagerRoleCountDTO::getRoleId,ManagerRoleCountDTO::getCount));
        //遍历对象，插入数据
        for (MRoleListDTO item:list) {
            Integer num=map.get(item.getRoleId());
            item.setAccountNum(num==null?0:num);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

    /**
     * @notes 通过角色类型获得角色
     * @Author junyang.li
     * @Date 9:21 2019/4/30
     **/
    @Override
    public List<MRoleInfo> getRolesByType(ManagerRoleType roleType){
        List<MRoleInfo> roles=getAllRoles();
        if(roleType==null){
            return roles;
        }
        int key=roleType.getKey();
        return roles.stream().map(obj->{
            if(obj.getRoleType()==key){
                return obj;
            }
            return null;
        }).filter(Objects::nonNull).collect(Collectors.toList());
    }

    /**
     * @notes: 根据角色id和权限类型获取角色
     * @Author: junyang.li
     * @Date: 14:01 2019/5/8
     * @param roleId : 角色id
     * @param type : 角色类型
     * @return: com.bee.platform.common.entity.MRoleInfo
     */
    @Override
    public MRoleInfo getRolesById(int roleId,ManagerRoleType type) {
        List<MRoleInfo> roles=getAllRoles();
        MRoleInfo roleInfo=null;
        for (MRoleInfo item:roles) {
            if(item.getRoleId()==roleId){
                roleInfo =item;
                break;
            }
        }
        //是否为空
        if(roleInfo==null){
            return null;
        }
        //如果类型不为空
        if(type!=null && type.getKey()!=roleInfo.getRoleType()){
            return null;
        }
        return roleInfo;
    }

    /**
     * @notes: 通过角色id和角色类型查询角色，返回角色对象
     * @Author: junyang.li
     * @Date: 16:24 2019/5/8
     * @param roleIds : 角色id集
     * @param type : 角色类型
     * @return: java.util.List<com.bee.platform.common.entity.MRoleInfo>
     */
    @Override
    public List<MRoleInfo> getRolesByIds(List<Integer> roleIds, ManagerRoleType type) {
        if(CollectionUtils.isEmpty(roleIds)){
            return getRolesByType(type);
        }else if(type==null){
            return getRolesByRoleIds(roleIds);
        }else{
            List<MRoleInfo> all=getAllRoles();
            Integer key=type.getKey();
            return all.stream().map(obj->{
                if(roleIds.contains(obj.getRoleId()) && key.equals(obj.getRoleType())){
                    return obj;
                }
                return null;
            }).filter(Objects::nonNull).collect(Collectors.toList());
        }
    }
    /**
     * @notes: 通过角色id和角色类型查询角色，返回角色id
     * @Author: junyang.li
     * @Date: 16:25 2019/5/8
     * @param roleIds : 角色id集
     * @param type : 角色类型
     * @return: java.util.List<java.lang.Integer>
     */
    @Override
    public List<Integer> getRoleIdsByIds(List<Integer> roleIds, ManagerRoleType type) {
        return getRolesByIds(roleIds,type).stream().map(MRoleInfo::getRoleId).collect(Collectors.toList());
    }

    /**
     * @notes 删除角色组
     * @Author junyang.li
     * @Date 11:06 2019/4/30
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> deleteRole(ManagerInfo managerInfo, Integer roleId) {
        //查询该角色是否存在
        MRoleInfo mRole=this.getRolesById(roleId,ManagerRoleType.COMMON);
        if(mRole==null){
            return ResponseResult.buildResponseResult(ResCodeEnum.MANAGER_ROLE_NOT_DELETE);
        }
        //查询该角色组中是否有成员
        Integer num=managersRolesMapper.countManagerByRoleId(roleId);
        if(num!=null && num>0){
            //不能删除
            String message= MessageFormat.format(ResCodeEnum.CAN_NOT_DELETE_MANAGER_ROLE.getMsg(),mRole.getRoleName());
            return ResponseResult.buildResponseResult(ResCodeEnum.CAN_NOT_DELETE_MANAGER_ROLE.getCode(),message);
        }
        //可以删除
        MRoles roles=new MRoles().setRoleId(roleId).setStatus(Status.FALSE.getKey()).setModifier(managerInfo.getNickname())
                .setModifyId(managerInfo.getManagerId()).setModifyTime(new Date());
        rolesMapper.updateById(roles);
        //增加操作日志
        String content=operatorLogService.createContent(OperatorLogType.DELETE_ROLE,managerInfo.getNickname(),mRole.getRoleName());
        operatorLogService.insetOperatorLog(managerInfo,content);
        //清除缓存
        jedisService.delKey(ConstantsUtil.MANAGER_ALL_ROLE);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * @notes 编辑角色组
     * @Author junyang.li
     * @Date 13:52 2019/4/30
     **/
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult<ResCodeEnum> editRole(ManagerInfo managerInfo, ManagerRoleVO vo) {
        //查出对应的角色
        List<Integer> roles=getRoleIdsByIds(vo.getRoleIds(),ManagerRoleType.BASIC);
        //参数有误
        if(CollectionUtils.isEmpty(roles)){
            log.info("编辑角色传入的角色id有误，无法查询到角色.{}",JSONObject.toJSONString(vo.getRoleIds()));
            return ResponseResult.buildResponseResult(ResCodeEnum.ROLE_ID_ERROR);
        }
        //父角色查子角色
        List<Integer> childIds=roleRoleService.getChildIdByParentIds(roles);
        if(!CollectionUtils.isEmpty(childIds)){
            //编辑权限默认用户查看权限
            childIds.forEach(obj->{
                if(!roles.contains(obj)){
                    roles.add(obj);
                }
            });
        }
        //新增
        return vo.getRoleId()==null?this.insertRole(managerInfo,vo,roles):this.updateRole(managerInfo,vo,roles);
    }


    /**
     * @notes: 根据角色id查询角色权限，roleId 为空则返回所有的基础权限
     * @Author: junyang.li
     * @Date: 15:59 2019/5/8
     * @param roleId :
     * @return: com.bee.platform.common.entity.ResponseResult<java.util.List<com.bee.platform.user.dto.MRoleGroupDTO>>
     */
    @Override
    public ResponseResult<List<MRoleGroupDTO>> getRoleDetail(Integer roleId) {
        //角色id不为空
        if(roleId!=null){
            //查询该角色是否存在
            MRoleInfo mRole=this.getRolesById(roleId,ManagerRoleType.COMMON);
            if(mRole==null){
                return ResponseResult.buildResponseResult(ResCodeEnum.MANAGER_ROLE_NOT_FOUND);
            }
        }
        //查询关联的基础角色
        List<MRoleGroupDTO> list= getBasicRole(roleId);
        //排序
        list.sort((MRoleGroupDTO o1, MRoleGroupDTO o2) ->(o1.getGroupId() - o2.getGroupId()));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list);
    }

    /**
     * @notes: 编辑基础角色的分组类别
     * @Author: junyang.li
     * @Date: 9:45 2019/5/13
     * @param managerInfo : 操作人信息
     * @param vo : 请求参数
     * @return: com.bee.platform.common.entity.ResponseResult<com.bee.platform.common.entity.ResCodeEnum>
     */
    @Override
    public ResponseResult<ResCodeEnum> updatePermission(ManagerInfo managerInfo, ManagerRoleVO vo) {
        return null;
    }


    /**
     * @notes: 根据角色id查询角色权限，roleId 为空则返回所有的基础权限
     * @Author: junyang.li
     * @Date: 15:59 2019/5/8
     * @param roleId :
     * @return: java.util.List<com.bee.platform.user.dto.MRoleGroupDTO>
     */
    private List<MRoleGroupDTO> getBasicRole(Integer roleId) {
        //获得所有的角色
        List<MRoleInfo> roles=getAllRoles();
        Map<Integer,MRoleInfo> map=new HashMap<>(16);
        //角色类型
        int basic=ManagerRoleType.BASIC_ROLE_GROUP.getKey();
        //遍历角色并且获取id
        List<Integer> roleIds=roles.stream().map(obj->{
            map.put(obj.getRoleId(),obj);
            if(obj.getRoleType()==basic){
                return obj.getRoleId();
            }
            return null;
        }).filter(Objects::nonNull).collect(Collectors.toList());
        //判空
        if(CollectionUtils.isEmpty(roleIds)){
            return new ArrayList<>();
        }
        //查出角色之间的关联关系
        List<MRoleRole> roleRoles=roleRoleService.selectList(new EntityWrapper<MRoleRole>().in("parent_role_id",roleIds));
        //获得需要查询的角色子角色id
        List<Integer> checkRole=roleRoleService.getChildIdByParentId(roleId);
        Map<Integer,MRoleGroupDTO> groups=new HashMap<>(16);
        //不为空
        for (MRoleRole obj:roleRoles) {
            MRoleInfo info=map.get(obj.getParentRoleId());
            MRoleGroupDTO parent=groups.get(info.getRoleId());
            //判空
            if(parent==null){
                parent=new MRoleGroupDTO().setGroupId(info.getRoleId()).setGroupName(info.getRoleName());
                //先传递引用
                groups.put(parent.getGroupId(),parent);
            }
            //再为引用赋值
            MRoleDTO role=BeanUtils.copyProperties(map.get(obj.getChildRoleId()),MRoleDTO.class);
            boolean check=checked(role.getRoleId(),checkRole);
            parent.isNull().add(role.setChecked(check));
        }
        return new ArrayList<>(groups.values());
    }


    /**
     * @notes: 判断基础角色是否被选中
     * @Author: junyang.li
     * @Date: 15:52 2019/5/8
     * @param roleId : 待校验角色id
     * @param checkdRole : 被选中的角色集
     * @return: boolean
     */
    private boolean checked(Integer roleId,List<Integer> checkdRole){
        if(CollectionUtils.isEmpty(checkdRole)){
            return false;
        }
        return checkdRole.contains(roleId);
    }


    /**
     * @notes 新增权限组
     * @Author junyang.li
     * @Date 15:12 2019/4/30
     **/
    private ResponseResult<ResCodeEnum> insertRole(ManagerInfo managerInfo, ManagerRoleVO vo,List<Integer> roles){
        //是否重名
        roleNameIsRepeat(vo.getRoleName());
        MRoles mRole=new MRoles().setRoleName(vo.getRoleName()).setRoleType(ManagerRoleType.COMMON.getKey())
                .setCreateId(managerInfo.getManagerId()).setCreateTime(new Date()).setCreator(managerInfo.getNickname())
                .setStatus(Status.TRUE.getKey()).setModifyTime(new Date());
        rolesMapper.insert(mRole);
        //查出对应的id
        mRole=rolesMapper.selectOne(new MRoles()
                .setRoleName(vo.getRoleName()).setStatus(Status.TRUE.getKey()));
        //添加关联关系
        List<MRoleRole> list=createRoleRole(roles,mRole.getRoleId());
        //添加基础角色
        roleRoleService.insertAll(list);
        //新增操作日志
        String content=operatorLogService.createContent(OperatorLogType.ADD_ROLE,managerInfo.getNickname(),vo.getRoleName());
        operatorLogService.insetOperatorLog(managerInfo,content);
        //清除缓存
        jedisService.delKey(ConstantsUtil.MANAGER_ALL_ROLE);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * @notes 修改权限组
     * @Author junyang.li
     * @Date 15:12 2019/4/30
     **/
    private ResponseResult<ResCodeEnum> updateRole(ManagerInfo managerInfo, ManagerRoleVO vo,List<Integer> roles){
        //编辑
        MRoles mRole=rolesMapper.selectOne(new MRoles()
                .setRoleId(vo.getRoleId()).setStatus(Status.TRUE.getKey()).setRoleType(ManagerRoleType.COMMON.getKey()));
        if(mRole==null){
            log.info("编辑角色组时，角色组id有误，无法查询到角色组{}",vo.getRoleId());
            return ResponseResult.buildResponseResult(ResCodeEnum.MANAGER_ROLE_NOT_FOUND);
        }
        //名称发生改变
        if(!mRole.getRoleName().equals(vo.getRoleName())){
            //查询名称是否重复
            roleNameIsRepeat(vo.getRoleName());
            rolesMapper.updateById(new MRoles().setRoleId(mRole.getRoleId()).setRoleName(vo.getRoleName()));
        }
        List<MRoleRole> list=createRoleRole(roles,mRole.getRoleId());
        //新增操作日志
        String content=operatorLogService.createContent(OperatorLogType.EDIT_ROLE,managerInfo.getNickname(),vo.getRoleName());
        operatorLogService.insetOperatorLog(managerInfo,content);
        //删除原有基础角色
        roleRoleService.delete(new EntityWrapper<MRoleRole>().where("parent_role_id={0}",mRole.getRoleId()));
        //添加基础角色
        roleRoleService.insertAll(list);
        systemNoticeService.insertNotice(managerInfo.getManagerId(), NoticeTemplateType.UPDATE_PERMISSION_GROUP,mRole.getRoleName());
        //清除缓存
        jedisService.delKey(ConstantsUtil.MANAGER_ALL_ROLE);
        jedisService.delKey(ConstantsUtil.ROLE_MENU_TREE+mRole.getRoleId());
        jedisService.delKey(ConstantsUtil.ROLE_BUTTON+mRole.getRoleId());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }


    /**
     * @notes: 查询角色名称是否重复
     * @Author: junyang.li
     * @Date: 16:51 2019/5/8
     * @param roleName : 待查询的角色名称
     * @return: void
     */
    private void roleNameIsRepeat(String roleName){
        MRoles mRole=rolesMapper.selectOne(new MRoles()
                .setRoleName(roleName).setStatus(Status.TRUE.getKey()));
        if(mRole!=null){
            log.info("权限组名称重复，无法添加。{}",JSONObject.toJSONString(mRole));
            throw new BusinessException(ResCodeEnum.MANAGER_ROLE_GROUP_EXIT, ExceptionMessageEnum.MANAGER_ROLE_GROUP_EXIT);
        }
    }

    /**
     * @notes 创建角色与角色之间的关联对象
     * @Author junyang.li
     * @Date 15:00 2019/4/30
     **/
    private List<MRoleRole> createRoleRole(List<Integer> roles,Integer parentRoleId){
        //创建关联对象
        return roles.stream().map(obj->{
            return new MRoleRole().setParentRoleId(parentRoleId).setChildRoleId(obj);
        }).collect(Collectors.toList());
    }

    /**
     * @notes 查询所有的角色
     * @Author junyang.li
     * @Date 16:56 2019/4/29
     **/
    private List<MRoles> allRoles(){
        return rolesMapper.selectList(new EntityWrapper<MRoles>().where("status=1").orderBy("role_id",false));
    }



}
