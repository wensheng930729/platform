package com.bee.platform.user.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.compress.utils.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.business.dto.PostListDTO;
import com.bee.platform.business.rq.PostAddRQ;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.Page;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.MiddleNoticeContentTemplate;
import com.bee.platform.common.exception.BusinessException;
import com.bee.platform.common.exception.ExceptionMessageEnum;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.PageUtils;
import com.bee.platform.user.constants.enums.EnumEnterpriseCheck;
import com.bee.platform.user.dao.mapper.DepartmentsMapper;
import com.bee.platform.user.dao.mapper.EnterprisesUsersMapper;
import com.bee.platform.user.dao.mapper.UsersDepartmentsMapper;
import com.bee.platform.user.dao.mapper.ZPostMapper;
import com.bee.platform.user.entity.Departments;
import com.bee.platform.user.entity.EnterprisesUsers;
import com.bee.platform.user.entity.UsersDepartments;
import com.bee.platform.user.entity.ZPost;
import com.bee.platform.user.service.MiddleSystemNoticeService;
import com.bee.platform.user.service.ZPostService;
import com.bee.platform.user.utils.DepartmentUtil;

import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 *  服务实现类
 * </p>
 *
 * @author hongchuan.he123
 * @since 2019-04-25
 */
@Slf4j
@Service
public class ZPostServiceImpl extends ServiceImpl<ZPostMapper, ZPost> implements ZPostService {

    @Autowired
    private DepartmentsServiceImpl departmentsService;
    @Autowired
    private DepartmentsMapper departmentsMapper;
    @Autowired
    private ZPostMapper postMapper;
    @Autowired
    private UsersDepartmentsMapper usersDepartmentsMapper;
    @Autowired
    private EnterprisesUsersMapper enterprisesUsersMapper;
    @Autowired
    private MiddleSystemNoticeService middleSystemNoticeService;

    @Override
    public ResponseResult<List<PostListDTO>> getPostList(AuthPlatformUserInfo userInfo, String name, Integer departmentId, Page page) {
        Pagination pagination= PageUtils.transFromPage(page);
        Integer orgId = userInfo.getOrgId();
        //当前用户公司下部门
        List<Departments> departmentsList = departmentsMapper.selectList(new EntityWrapper<Departments>().eq("org_id", orgId));
        if(CollectionUtils.isEmpty(departmentsList)){
            log.info("部门信息为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,Lists.newArrayList(),PageUtils.transToPage(pagination));
        }
        List<Integer> departmentIds = departmentsList.stream().map(e -> e.getId()).collect(Collectors.toList());
        EntityWrapper<ZPost> postWrapper = new EntityWrapper<>();
        List<Departments> departments = departmentsMapper.selectList(new EntityWrapper<>());
        //部门范围：查询带出部门或企业下所有部门
        if(!ObjectUtils.isEmpty(departmentId)){
            if(!CollectionUtils.isEmpty(departments)){
                ArrayList<Integer> list = new ArrayList<>();
                list.add(departmentId);
                List<Integer> result = DepartmentUtil.getTree(departments, departmentId, list);
                postWrapper.in("department_id",result);
            }
        }else{
            postWrapper.in("department_id",departmentIds);
        }
        List<PostListDTO> list = new ArrayList<>();
        List<ZPost> posts = postMapper.selectPage(pagination,postWrapper
                .like("name", name)

                .eq("status", EnumCommon.IsActive.is_active.getKey()));
        if(!CollectionUtils.isEmpty(posts)){
            for (ZPost post : posts){
                PostListDTO dto = BeanUtils.copyProperties(post, PostListDTO.class);
                //部门层级
                Integer depId = dto.getDepartmentId();
                if(!ObjectUtils.isEmpty(depId) && !ObjectUtils.isEmpty(departmentsMapper.selectById(depId))){
                    dto.setLevel(departmentsMapper.selectById(depId).getLevel());
                }
                //所属关系
                String relation = departmentsService.getDepartmentLevelWithSelf(post.getDepartmentId());
                dto.setRelation(relation);
                //该职位下是否挂有职员
                Integer postId = post.getId();
                List<UsersDepartments> users1 = usersDepartmentsMapper.selectList(
                        new EntityWrapper<UsersDepartments>().eq("post_id", postId));
                List<EnterprisesUsers> users2 = enterprisesUsersMapper.selectList(new EntityWrapper<EnterprisesUsers>().eq("zpostid", postId));
                dto.setDeletable((CollectionUtils.isEmpty(users1)&&CollectionUtils.isEmpty(users2))
                        ?EnumCommon.IsActive.is_active.getKey()
                        :EnumCommon.IsActive.not_active.getKey());
                list.add(dto);
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS,list,PageUtils.transToPage(pagination));
    }

    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult editPost(AuthPlatformUserInfo userInfo, PostAddRQ rq) {
        String name = rq.getName();
        Integer departmentId = rq.getDepartmentId();
        //所选部门是否属于当前企业
        Integer orgId = userInfo.getOrgId();
        List<Departments> departmentsList = departmentsMapper.selectList(new EntityWrapper<Departments>().eq("org_id", orgId));
        if(CollectionUtils.isEmpty(departmentsList)){
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.DEPARTMENT_FIND_FAILED);
        }
        List<Integer> departmentIds = departmentsList.stream().map(e -> e.getId()).collect(Collectors.toList());
        if(!departmentIds.contains(departmentId)){
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM,ExceptionMessageEnum.DEPARTMENT_NOT_TO_THIS);
        }
        List<ZPost> posts = Lists.newArrayList();
        if (rq.getId() == null){
            posts = postMapper.selectList(new EntityWrapper<ZPost>()
                    .eq("department_id", departmentId)
                    .eq("name", name)
                    .eq("status", EnumCommon.IsActive.is_active.getKey()));
        }else {
            posts = postMapper.selectList(new EntityWrapper<ZPost>()
                    .ne("id",rq.getId())
                    .eq("department_id", departmentId)
                    .eq("name", name)
                    .eq("status", EnumCommon.IsActive.is_active.getKey()));
        }

        if(!CollectionUtils.isEmpty(posts)){
            throw new BusinessException(ResCodeEnum.ERROR_SYSTEM, ExceptionMessageEnum.POST_NAME_EXISTED);
        }
        //新增
        if(ObjectUtils.isEmpty(rq.getId())){
            ZPost post = new ZPost();
            post.setDepartmentId(departmentId);
            post.setDescription(rq.getDescription());
            post.setName(name);
            post.setStatus(EnumCommon.IsActive.is_active.getKey());
            if(postMapper.insert(post)<=0){
                throw new BusinessException(ResCodeEnum.POST_SAVE_FAILED, ExceptionMessageEnum.POST_SAVE_FAILED);
            }
            //中台系统通知
            String title = EnumMiddleNoticeTitle.title.ADD_POSITION_NOTICE.getValue();
            middleSystemNoticeService.createNotice(userInfo.getId(),title, MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_ADD_POST.getKey(),userInfo.getOrg_name(), name);
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
        }
        //修改
        ZPost post = postMapper.selectOne(new ZPost().setId(rq.getId()));
        if(ObjectUtils.isEmpty(post)){
            return ResponseResult.buildResponseResult(ResCodeEnum.POST_NOT_EXIST);
        }
        post.setName(name);
        post.setDescription(rq.getDescription());
        post.setDepartmentId(departmentId);
        if(postMapper.updateById(post)<=0){
            throw new BusinessException(ResCodeEnum.POST_SAVE_FAILED, ExceptionMessageEnum.POST_SAVE_FAILED);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult deletePost(AuthPlatformUserInfo userInfo, Integer id) {
        ZPost post = postMapper.selectOne(new ZPost().setId(id));
        if(ObjectUtils.isEmpty(post)){
            return ResponseResult.buildResponseResult(ResCodeEnum.POST_NOT_EXIST);
        }
        String postName = post.getName();
        post.setStatus(EnumCommon.IsActive.not_active.getKey());
        if(postMapper.updateById(post)<=0){
            throw new BusinessException(ResCodeEnum.POST_SAVE_FAILED, ExceptionMessageEnum.POST_SAVE_FAILED);
        }
        //中台系统通知
        String title = EnumMiddleNoticeTitle.title.DELETE_POSITION_NOTICE.getValue();
        middleSystemNoticeService.createNotice(userInfo.getId(),title, MiddleNoticeContentTemplate.ENTERPRISE_ADMIN_DELETE_POST.getKey(),userInfo.getOrg_name(), postName);
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    @Override
    public ResponseResult<Integer> postNameCheck(AuthPlatformUserInfo userInfo, String name, Integer departmentId) {
        List<ZPost> posts = postMapper.selectList(new EntityWrapper<ZPost>()
                .eq("department_id", departmentId)
                .eq("name", name)
                .eq("status", EnumCommon.LogicStatus.NORMAL.getKey()));
        if(CollectionUtils.isEmpty(posts)){
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, EnumEnterpriseCheck.IS_EXISTED.NO.getKey());
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, EnumEnterpriseCheck.IS_EXISTED.YES.getKey());
    }

    @Override
    public ResponseResult<List<ZPost>> getByDepartment(Integer departmentId) {
        if (ObjectUtils.isEmpty(departmentId)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.PARAMETER_INCOMPLETE);
        }
        List<ZPost> posts = postMapper.selectList(new EntityWrapper<>(new ZPost().setDepartmentId(departmentId)));
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, posts);
    }


    @Override
    public PostListDTO getPostById(Integer id) {
        ZPost zPost = selectById(id);
        String relation="";
        if(!ObjectUtils.isEmpty(zPost)){
            //所属关系
            relation  = departmentsService.getDepartmentLevelWithId(zPost.getDepartmentId());

        }
        PostListDTO dto = BeanUtils.copyProperties(zPost, PostListDTO.class);
        dto.setRelation(relation);

        return dto;
    }
}
