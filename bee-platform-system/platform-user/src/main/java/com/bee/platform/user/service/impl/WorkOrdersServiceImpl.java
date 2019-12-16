package com.bee.platform.user.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ObjectUtils;
import org.springframework.util.StringUtils;

import com.alibaba.fastjson.JSONObject;
import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.baomidou.mybatisplus.plugins.pagination.Pagination;
import com.baomidou.mybatisplus.service.impl.ServiceImpl;
import com.bee.platform.common.constants.enums.EnumCommon;
import com.bee.platform.common.constants.enums.EnumMiddleNoticeTitle;
import com.bee.platform.common.constants.enums.EnumWorkOrders;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.enums.EnumRoleType;
import com.bee.platform.common.enums.MiddleNoticeContentTemplate;
import com.bee.platform.common.enums.NoticeTemplateType;
import com.bee.platform.common.enums.Status;
import com.bee.platform.common.service.GenerateIdService;
import com.bee.platform.common.utils.BeanUtils;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.StringUtil;
import com.bee.platform.user.authority.dao.mapper.AuthPlatformUserMapper;
import com.bee.platform.user.authority.dao.mapper.AuthUserRoleMapper;
import com.bee.platform.user.authority.entity.AuthUserRole;
import com.bee.platform.user.dao.mapper.AppMapper;
import com.bee.platform.user.dao.mapper.WorkOrdersDetailMapper;
import com.bee.platform.user.dao.mapper.WorkOrdersMapper;
import com.bee.platform.user.dto.AttachmentDTO;
import com.bee.platform.user.dto.NeedToBeDealtDTO;
import com.bee.platform.user.dto.NeedToBeDealtDetailDTO;
import com.bee.platform.user.dto.WorkOrdersDTO;
import com.bee.platform.user.dto.WorkOrdersDetailDTO;
import com.bee.platform.user.entity.App;
import com.bee.platform.user.entity.SystemNotice;
import com.bee.platform.user.entity.WorkOrders;
import com.bee.platform.user.entity.WorkOrdersDetail;
import com.bee.platform.user.rq.MiddleSystemNoticeRQ;
import com.bee.platform.user.rq.WorkOrdersInfoRQ;
import com.bee.platform.user.rq.WorkOrdersReplyRQ;
import com.bee.platform.user.service.MiddleSystemNoticeService;
import com.bee.platform.user.service.SystemNoticeService;
import com.bee.platform.user.service.WorkOrdersService;
import com.bee.platform.user.sms.SmsService;
import com.bee.platform.user.utils.ValidateUtils;

import lombok.extern.slf4j.Slf4j;

/**
 * <p>
 * 工单信息表 服务实现类
 * </p>
 *
 * @author qhwang
 * @since 2019-04-25
 */
@Slf4j
@Service
public class WorkOrdersServiceImpl extends ServiceImpl<WorkOrdersMapper, WorkOrders> implements WorkOrdersService {

    @Autowired
    private WorkOrdersMapper workOrdersMapper;

    @Autowired
    private WorkOrdersDetailMapper workOrdersDetailMapper;

    @Autowired
    private GenerateIdService generateIdService;

    @Autowired
    private AppMapper appMapper;

    @Autowired
    private SmsService smsService;

    @Autowired
    private MiddleSystemNoticeService middleSystemNoticeService;

    //@Autowired
    //private UsersService usersService;

    @Autowired
    private SystemNoticeService systemNoticeService;

    //@Autowired
    //private MRoleRoleMapper mRoleRoleMapper;

    @Autowired
    private AuthUserRoleMapper userRoleMapper;

    @Autowired
    private AuthPlatformUserMapper userMapper;

    /**
     * 根据条件查询工单信息列表
     *
     * @param workOrdersNumber
     * @param startTime
     * @param endTime
     * @param priority
     * @param orderStatus
     * @param page
     * @param userOrder
     * @return
     */
    @Override
    public List<WorkOrdersDTO> getWorkOrdersList(String workOrdersNumber, String startTime, String endTime, String priority, String orderStatus, AuthPlatformUserInfo userInfo, Pagination page, Boolean userOrder) {
        List<WorkOrders> workOrdersList = new ArrayList<>();
        List<WorkOrdersDTO> workOrdersDTOS = new ArrayList<>();
        EntityWrapper entityWrapper = new EntityWrapper<WorkOrders>();
        if (ObjectUtils.isEmpty(userInfo)){
            //工单编号
            if (!StringUtils.isEmpty(workOrdersNumber)) {
                entityWrapper.like("work_order_number", workOrdersNumber);
            }
            //日期
            if (!StringUtils.isEmpty(startTime) && !StringUtils.isEmpty(endTime)) {
                startTime += " 00:00:00";
                endTime += " 23:59:59";
                entityWrapper.between("submit_time", startTime, endTime);
            }
            //优先级
            if (!StringUtils.isEmpty(priority)) {
                entityWrapper.eq("priority", priority);
            }
            //工单状态
            if (!StringUtils.isEmpty(orderStatus)) {
                entityWrapper.eq("order_status", orderStatus);
            }
            //排序
            List list = new ArrayList<String>();
            //是否是中台用户查询，决定排序规则
            if (userOrder) {
                list.add("priority");
            }
            list.add("submit_time");
            entityWrapper.orderDesc(list);

            //分页查询工单信息
            workOrdersList = workOrdersMapper.selectPage(page, entityWrapper);

            workOrdersDTOS = BeanUtils.assemble(WorkOrdersDTO.class, workOrdersList);
        }else {
            String s = "";
            //查询用户在当前企业中的角色
            List<AuthUserRole> authUserRoles = userRoleMapper.selectList(new EntityWrapper<AuthUserRole>()
                    .eq("user_id", userInfo.getId())
                    .eq("enterprise_id", userInfo.getOrgId())
                    .eq("deleted", Status.FALSE.getKey()));
            for (AuthUserRole role : authUserRoles){
                s = s + "," + role.getRoleType();
            }
            if (Arrays.asList(s.split(",")).contains(EnumRoleType.ENTERPRISE_ADMIN.getCode())){
                //所属公司
                entityWrapper.eq("belong_company", userInfo.getOrgId());
            }else {
                //所属人
                entityWrapper.eq("create_id", userInfo.getId());
            }
            //工单编号
            if (!StringUtils.isEmpty(workOrdersNumber)) {
                entityWrapper.like("work_order_number", workOrdersNumber);
            }
            //日期
            if (!StringUtils.isEmpty(startTime) && !StringUtils.isEmpty(endTime)) {
                startTime += " 00:00:00";
                endTime += " 23:59:59";
                entityWrapper.between("submit_time", startTime, endTime);
            }
            //优先级
            if (!StringUtils.isEmpty(priority)) {
                entityWrapper.eq("priority", priority);
            }
            //工单状态
            if (!StringUtils.isEmpty(orderStatus)) {
                entityWrapper.eq("order_status", orderStatus);
            }
            //排序
            List list = new ArrayList<String>();
            //是否是中台用户查询，决定排序规则
            if (userOrder) {
                list.add("priority");
            }
            list.add("submit_time");
            entityWrapper.orderDesc(list);

            //分页查询工单信息
            workOrdersList = workOrdersMapper.selectPage(page, entityWrapper);

            workOrdersDTOS = BeanUtils.assemble(WorkOrdersDTO.class, workOrdersList);
        }
        return workOrdersDTOS;
    }

    /**
     * 根据工单编号查询工单详情
     *
     * @param workOrdersNumber
     * @return
     */
    @Override
    public ResponseResult<WorkOrdersDTO> getWorkOrdersDetail(String workOrdersNumber) {

        //查询工单信息
        WorkOrders workOrders = workOrdersMapper.selectOne(new WorkOrders().setWorkOrderNumber(workOrdersNumber));
        if (ObjectUtils.isEmpty(workOrders)) {
            log.error("工单信息不存在！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        WorkOrdersDTO workOrdersDTO = BeanUtils.copyProperties(workOrders, WorkOrdersDTO.class);

        //工单关闭时，将更新时间作为问题解决时间
        if (EnumWorkOrders.WORK_ORDER_STATUS.close.getKey().equals(workOrders.getOrderStatus())) {
            workOrdersDTO.setProblemSolvingTime(workOrders.getModifyTime());
        }

        //处理附件信息
        String enclosureJson = workOrders.getEnclosure();
        if (!StringUtils.isEmpty(enclosureJson)) {
            List<AttachmentDTO> enclosure = JSONObject.parseArray(enclosureJson, AttachmentDTO.class);
            workOrdersDTO.setEnclosure(enclosure);
        }

        //查询工单处理详情
        List<WorkOrdersDetail> workOrdersDetails = workOrdersDetailMapper.selectList(new EntityWrapper<WorkOrdersDetail>()
                .eq("work_order_number", workOrdersNumber).orderBy("reply_time", true));
        List<WorkOrdersDetailDTO> workOrdersDetailDTOS = new ArrayList<>();
        for (WorkOrdersDetail workOrdersDetail : workOrdersDetails) {
            WorkOrdersDetailDTO workOrdersDetailDTO = BeanUtils.copyProperties(workOrdersDetail, WorkOrdersDetailDTO.class);
            //处理附件信息
            String enclosureStr = workOrdersDetail.getEnclosure();
            if (!StringUtils.isEmpty(enclosureStr)) {
                List<AttachmentDTO> enclosureList = JSONObject.parseArray(enclosureStr, AttachmentDTO.class);
                workOrdersDetailDTO.setEnclosure(enclosureList);
            }
            workOrdersDetailDTOS.add(workOrdersDetailDTO);
        }
        workOrdersDTO.setDetailDTOS(workOrdersDetailDTOS);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, workOrdersDTO);
    }

    /**
     * 工单沟通回复(用户询问)
     * @param workOrdersReplyRQ
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult replyWorkOrders(WorkOrdersReplyRQ workOrdersReplyRQ, AuthPlatformUserInfo userInfo) {

        //查询工单信息
        WorkOrders workOrders = workOrdersMapper.selectOne(new WorkOrders().setWorkOrderNumber(workOrdersReplyRQ.getWorkOrderNumber()));
        if (ObjectUtils.isEmpty(workOrders)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        //判断工单是否关闭
        if (EnumWorkOrders.WORK_ORDER_STATUS.close.getKey().equals(workOrders.getOrderStatus())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.WORK_ORDER_HAS_CLOSED);
        }

        //保存回复信息
        WorkOrdersDetail workOrdersDetail = new WorkOrdersDetail();
        workOrdersDetail.setWorkOrderNumber(workOrdersReplyRQ.getWorkOrderNumber());
        workOrdersDetail.setReplyDescription(workOrdersReplyRQ.getReplyDescription());
        //处理附件信息
        List<AttachmentDTO> enclosure = workOrdersReplyRQ.getEnclosure();
        if (!CollectionUtils.isEmpty(enclosure)) {
            String enclosureJson = JSONObject.toJSONString(enclosure);
            workOrdersDetail.setEnclosure(enclosureJson);
        }
        workOrdersDetail.setReplyId(userInfo.getId().longValue());
        workOrdersDetail.setReplyHead(userInfo.getHead());
        workOrdersDetail.setReplyName(userInfo.getNickname());
        workOrdersDetail.setReplyTime(new Date());
        if (workOrdersDetailMapper.insert(workOrdersDetail) < 1) {
            log.error("保存工单回复信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //修改工单状态
        //提交人回复 工单状态：平台处理中
        workOrders.setOrderStatus(EnumWorkOrders.WORK_ORDER_STATUS.doing.getKey());

        workOrders.setModifyId(userInfo.getId().longValue());
        workOrders.setModifier(userInfo.getNickname());
        workOrders.setModifyTime(new Date());
        if (workOrdersMapper.updateById(workOrders) < 1) {
            log.error("保存工单信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        //保存后台消息
        /*List<Integer> roles = mRoleRoleMapper.getManagerIdsByChildId(18);
        int arr[] = roles.stream().mapToInt(Integer::byteValue).toArray();*/
        List<AuthUserRole> userRoles = userRoleMapper.selectList(new EntityWrapper<>(new AuthUserRole()
                .setRoleType("super_admin").setDeleted(Status.FALSE.getKey())));
        List<Integer> roles = userRoles.stream().map(a -> a.getUserId()).collect(Collectors.toList());
        int arr[] = roles.stream().mapToInt(Integer::byteValue).toArray();

        List<SystemNotice> systemNotices = systemNoticeService.createNotice(arr, NoticeTemplateType.WORK_ORDER_REPLY,
                userInfo.getNickname(), workOrders.getWorkOrderTitle());
        // 插入系统通知
        try {
            systemNoticeService.insertAll(systemNotices);
        }catch (Exception e){
            log.error("新增后台系统通知信息失败,异常信息{}",e);
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 工单沟通回复(工作人员答复)
     * @param workOrdersReplyRQ
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult replyAnswerWorkOrders(WorkOrdersReplyRQ workOrdersReplyRQ, AuthPlatformUserInfo userInfo) {

        //查询工单信息
        WorkOrders workOrders = workOrdersMapper.selectOne(new WorkOrders().setWorkOrderNumber(workOrdersReplyRQ.getWorkOrderNumber()));
        if (ObjectUtils.isEmpty(workOrders)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        //判断工单是否关闭
        if (EnumWorkOrders.WORK_ORDER_STATUS.close.getKey().equals(workOrders.getOrderStatus())) {
            return ResponseResult.buildResponseResult(ResCodeEnum.WORK_ORDER_HAS_CLOSED);
        }

        //保存回复信息
        WorkOrdersDetail workOrdersDetail = new WorkOrdersDetail();
        workOrdersDetail.setWorkOrderNumber(workOrdersReplyRQ.getWorkOrderNumber());
        workOrdersDetail.setReplyDescription(workOrdersReplyRQ.getReplyDescription());
        //处理附件信息
        List<AttachmentDTO> enclosure = workOrdersReplyRQ.getEnclosure();
        if (!CollectionUtils.isEmpty(enclosure)) {
            String enclosureJson = JSONObject.toJSONString(enclosure);
            workOrdersDetail.setEnclosure(enclosureJson);
        }
        workOrdersDetail.setReplyId(userInfo.getId().longValue());
        // 设置金蜜默认头像
        workOrdersDetail.setReplyHead(ConstantsUtil.DEFAULT_HEAD);
        workOrdersDetail.setReplyName(userInfo.getNickname());
        workOrdersDetail.setReplyTime(new Date());
        if (workOrdersDetailMapper.insert(workOrdersDetail) < 1) {
            log.error("保存工单回复信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //修改工单状态
        //受理人回复 工单状态：待用户确认
        workOrders.setOrderStatus(EnumWorkOrders.WORK_ORDER_STATUS.confirm.getKey());
        workOrders.setRecentAcceptTime(new Date());
        workOrders.setAcceptId(userInfo.getId().longValue());
        workOrders.setAcceptor(userInfo.getNickname());

        workOrders.setModifyId(userInfo.getId().longValue());
        workOrders.setModifier(userInfo.getNickname());
        workOrders.setModifyTime(new Date());

        if (workOrdersMapper.updateById(workOrders) < 1) {
            log.error("保存工单信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }

        //保存消息
        //User user = usersService.selectOne(new User().setUsername(workOrders.getPhone()));
        middleSystemNoticeService.createNotice(Integer.valueOf(workOrders.getCreateId().toString()),
                EnumMiddleNoticeTitle.title.WORK_ORDER_STATUS_CHANGE.getValue(),
                MiddleNoticeContentTemplate.WORK_ORDER_RESPONSED.getKey(),
                new Object[]{workOrders.getWorkOrderTitle()});

        //短信通知新待办事项生成
        String replyDescription = StringUtil.splice(workOrdersReplyRQ.getReplyDescription(), 10);
        //统计待办工单数量
        int countTodoWorkOrders = workOrdersMapper.selectCount(new EntityWrapper<>(new WorkOrders()
                .setOrderStatus(EnumWorkOrders.WORK_ORDER_STATUS.confirm.getKey())
                .setCreateId(workOrders.getCreateId().longValue()).setStatus(Status.TRUE.getKey())));
        //发送短信
        sendSmsAfterCreateToDoItem(workOrders.getPhone(), replyDescription, countTodoWorkOrders,
                NoticeTemplateType.TO_DO_ITEM_GENERATION.getKey());
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 新建工单
     *
     * @param workOrdersInfoRQ
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult createWorkOrders(WorkOrdersInfoRQ workOrdersInfoRQ, AuthPlatformUserInfo userInfo) {
        WorkOrders workOrders = new WorkOrders();
        workOrders = BeanUtils.copyProperties(workOrdersInfoRQ, workOrders);

        //工单编号
        App app = appMapper.selectById(workOrdersInfoRQ.getBelongApp());
        if (ObjectUtils.isEmpty(app)) {
            log.error("产品信息不存在！");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        String workOrderNumber = app.getAbbreviation() + generateIdService.getWorkOrderNumber();
        workOrders.setWorkOrderNumber(workOrderNumber);
        workOrders.setBelongAppName(app.getName());

        //处理附件信息
        List<AttachmentDTO> enclosure = workOrdersInfoRQ.getEnclosure();
        if (!CollectionUtils.isEmpty(enclosure)) {
            String enclosureJson = JSONObject.toJSONString(enclosure);
            workOrders.setEnclosure(enclosureJson);
        }

        workOrders.setOrderStatus(EnumWorkOrders.WORK_ORDER_STATUS.todo.getKey());
        workOrders.setStatus(EnumCommon.IsActive.is_active.getKey());
        workOrders.setSubmitTime(new Date());
        workOrders.setBelongCompany(userInfo.getOrgId());
        workOrders.setBelongCompanyName(userInfo.getOrg_name());
        workOrders.setRoleName(userInfo.getRoleList().get(0).getRoleName());
        workOrders.setCreateId(userInfo.getId().longValue());
        workOrders.setCreateHead(userInfo.getHead());
        workOrders.setCreator(userInfo.getNickname());
        workOrders.setCreateTime(new Date());

        if (workOrdersMapper.insert(workOrders) < 1) {
            log.error("新建工单信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        // 保存中台消息
        MiddleSystemNoticeRQ middleSystemNoticeRQ = new MiddleSystemNoticeRQ();
        middleSystemNoticeService.createNotice(userInfo.getId(),EnumMiddleNoticeTitle.title.CREATE_WORK_ORDER.getValue(),
                MiddleNoticeContentTemplate.WORK_ORDER_CREATE.getKey(),workOrdersInfoRQ.getWorkOrderTitle());

        // 后台消息
        /*List<Integer> roles = mRoleRoleMapper.getManagerIdsByChildId(18);
        int arr[] = roles.stream().mapToInt(Integer::byteValue).toArray();*/
        List<AuthUserRole> userRoles = userRoleMapper.selectList(new EntityWrapper<>(new AuthUserRole()
                .setRoleType("super_admin").setDeleted(Status.FALSE.getKey())));
        List<Integer> roles = userRoles.stream().map(a -> a.getUserId()).collect(Collectors.toList());
        int arr[] = roles.stream().mapToInt(Integer::byteValue).toArray();

        List<SystemNotice> systemNotices = systemNoticeService.createNotice(arr, NoticeTemplateType.WORK_ORDER_APPLY,
                userInfo.getNickname(), workOrdersInfoRQ.getWorkOrderTitle());

        // 插入系统通知
        try {
            systemNoticeService.insertAll(systemNotices);
        }catch (Exception e){
            log.error("新增后台系统通知信息失败,异常信息{}",e);
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 确认工单已经解决
     * @param workOrdersReplyRQ
     * @param userInfo
     * @return
     */
    @Override
    @Transactional(rollbackFor = Exception.class)
    public ResponseResult doneWorkOrders(WorkOrdersReplyRQ workOrdersReplyRQ, AuthPlatformUserInfo userInfo) {
        //查询工单信息
        WorkOrders workOrders = workOrdersMapper.selectOne(new WorkOrders().setWorkOrderNumber(workOrdersReplyRQ.getWorkOrderNumber()));
        if (ObjectUtils.isEmpty(workOrders)) {
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_NOT_FOUND);
        }
        //修改工单状态为已关闭
        workOrders.setOrderStatus(EnumWorkOrders.WORK_ORDER_STATUS.close.getKey());
        workOrders.setModifyId(userInfo.getId().longValue());
        workOrders.setModifier(userInfo.getNickname());
        workOrders.setModifyTime(new Date());
        if (workOrdersMapper.updateById(workOrders) < 1) {
            log.error("保存工单信息失败！");
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        // 保存中台消息
        MiddleSystemNoticeRQ middleSystemNoticeRQ = new MiddleSystemNoticeRQ();
        middleSystemNoticeService.createNotice(userInfo.getId(),EnumMiddleNoticeTitle.title.WORK_ORDER_CLOSE.getValue(),
                MiddleNoticeContentTemplate.WORK_ORDER_SOLVED.getKey(),workOrders.getWorkOrderTitle());

        // 后台消息
        /*List<Integer> roles = mRoleRoleMapper.getManagerIdsByChildId(18);
        int arr[] = roles.stream().mapToInt(Integer::byteValue).toArray();*/
        List<AuthUserRole> userRoles = userRoleMapper.selectList(new EntityWrapper<>(new AuthUserRole()
                .setRoleType("super_admin").setDeleted(Status.FALSE.getKey())));
        List<Integer> roles = userRoles.stream().map(a -> a.getUserId()).collect(Collectors.toList());
        int arr[] = roles.stream().mapToInt(Integer::byteValue).toArray();
        List<SystemNotice> systemNotices = systemNoticeService.createNotice(arr, NoticeTemplateType.WORK_ORDER_CLOSE,
                userInfo.getNickname(), workOrders.getWorkOrderTitle());
        // 插入系统通知
        try {
            systemNoticeService.insertAll(systemNotices);
        }catch (Exception e){
            log.error("新增后台系统通知信息失败,异常信息{}",e);
            return ResponseResult.buildResponseResult(ResCodeEnum.SAVE_FAIL);
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS);
    }

    /**
     * 查询待办的任务信息
     * @param pageSize
     * @param companyId
     * @return
     */
    @Override
    public ResponseResult getNeedToBeDealtInfo(Integer pageSize, Integer companyId) {

        NeedToBeDealtDTO needToBeDealtDTO = new NeedToBeDealtDTO();
        List<NeedToBeDealtDetailDTO> detailDTOS = new ArrayList<>();

        //如果没传显示条数，默认为显示5条待办信息
        if (ObjectUtils.isEmpty(pageSize)) {
            pageSize = 5;
        }

        //查询用户相关工单待办列表
        List<WorkOrders> workOrders = this.selectList(new EntityWrapper<WorkOrders>()
                .eq("order_status", EnumWorkOrders.WORK_ORDER_STATUS.confirm.getKey())
                .eq("belong_company", companyId)
                .eq("status", EnumCommon.IsActive.is_active.getKey())
                .orderBy("modify_time", false));

        needToBeDealtDTO.setTaskNum(workOrders.size());

        //遍历，需要几条取几条
        for (int i=0; i<workOrders.size(); i++) {
            NeedToBeDealtDetailDTO needToBeDealtDetailDTO = new NeedToBeDealtDetailDTO();
            needToBeDealtDetailDTO.setTaskNumber(workOrders.get(i).getWorkOrderNumber());
            needToBeDealtDetailDTO.setTaskTitle(workOrders.get(i).getWorkOrderTitle());
            needToBeDealtDetailDTO.setTaskContent(workOrders.get(i).getProblemDescription());
            if (!ObjectUtils.isEmpty(workOrders.get(i).getRecentAcceptTime())) {
                needToBeDealtDetailDTO.setHandleTime(workOrders.get(i).getRecentAcceptTime());
            } else if (!ObjectUtils.isEmpty(workOrders.get(i).getModifyTime())) {
                needToBeDealtDetailDTO.setHandleTime(workOrders.get(i).getModifyTime());
            } else {
                needToBeDealtDetailDTO.setHandleTime(workOrders.get(i).getCreateTime());
            }
            needToBeDealtDetailDTO.setTaskType(1);
            detailDTOS.add(needToBeDealtDetailDTO);
            if (pageSize.equals(i+1)) {
                break;
            }
        }
        needToBeDealtDTO.setDetailDTOS(detailDTOS);

        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, needToBeDealtDTO);
    }


    /**
     * @Description 工单状态变化后 发送短信
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    private void sendSmsAfterReplyAnswerWorkOrders(String phone, String workOrderTitle, Integer key) {
        try {
            if (!ValidateUtils.isPhone(phone)) {
                log.info(ResCodeEnum.NOT_PHONE_NUMBER.msg);
            }
            //组装参数
            JSONObject sendJson = new JSONObject();
            sendJson.put("work_order_title", workOrderTitle);
            smsService.sendMessageForPrompt(phone, key, sendJson.toJSONString());
        } catch (Exception e) {
            log.info(e.getMessage());
        }
    }

    /**
     * @Description 生成新待办事项后，发送短信
     * @Param phone
     * @Param item
     * @Param count
     * @Param key
     * @Return
     * @Date 2019/5/13 15:12
     * @Author xin.huang
     */
    private void sendSmsAfterCreateToDoItem(String phone, String item, Integer count, Integer key) {
        try {
            if (!ValidateUtils.isPhone(phone)) {
                log.info(ResCodeEnum.NOT_PHONE_NUMBER.msg);
            }
            //组装参数
            JSONObject sendJson = new JSONObject();
            sendJson.put("content_entrusted_item", item);
            sendJson.put("count", count);
            smsService.sendMessageForPrompt(phone, key, sendJson.toJSONString());
        } catch (Exception e) {
            log.info(e.getMessage());
        }
    }
}
